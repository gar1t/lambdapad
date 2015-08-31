%%% Copyright 2015 Garrett Smith <g@rre.tt>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(lpad_errors).

-export([handle_error/1]).

handle_error({{template_compile, _, Error}, _}) ->
    handle_template_compile_error(Error);
handle_error({{index_compile, _, Error, _}, _}) ->
    handle_index_compile_error(Error);
handle_error({{eterm_source, File, Error}, _}) ->
    handle_eterm_source_error(File, Error);
handle_error({Reason, [{index, _, _, _}=Location|_]=Stack}) ->
    handle_index_error(Reason, Location, Stack);
handle_error(Other) ->
    log_general_error("ERROR", Other).

handle_template_compile_error({T, [{Line, erlydtl_scanner, Msg}]}) ->
    log_line_error(T, Line, Msg);
handle_template_compile_error({T, [{{Line, Col}, erlydtl_parser, Msg}]}) ->
    log_line_error(T, Line, Col, Msg);
handle_template_compile_error({File, [{0, T, Msg}]}) ->
    log_line_error(T, 99999999, format_malformed_file_error(File, Msg));
handle_template_compile_error(Other) ->
    log_general_error("TEMPLATE COMPILE ERROR", Other).

format_malformed_file_error(File, Msg) ->
    [Msg, ": ", File].

handle_index_compile_error(Errors) when is_list(Errors) ->
    lists:foreach(fun log_index_error/1, Errors);
handle_index_compile_error([{File, Errors}]) ->
    lists:foreach(fun(Err) -> log_index_error(File, Err) end, Errors);
handle_index_compile_error(Error) ->
    log_general_error("INDEX COMPILE ERROR", Error).

handle_eterm_source_error(File, {Line, erl_parse, Msg}) ->
    log_line_error(File, Line, Msg).

handle_index_error(Reason, {index, _, _, FileInfo}, Stack) ->
    File = proplists:get_value(file, FileInfo),
    Line = proplists:get_value(line, FileInfo),
    log_line_error(File, Line, format_error_reason(Reason)),
    lpad_log:log_error("~p", [Stack]).

log_index_error({File, Errors}) ->
    lists:foreach(fun(Err) -> log_index_error(File, Err) end, Errors).

log_index_error(File, {Line, erl_parse, Msg}) ->
    log_line_error(File, Line, Msg);
log_index_error(File, {Line, erl_lint, Reason}) ->
    log_line_error(File, Line, format_error_reason(Reason));
log_index_error(File, Other) ->
    log_general_error("INDEX ERROR", {File, Other}).

format_error_reason({undefined_function, {Name, Arity}}) ->
    io_lib:format("undefined function ~s/~b", [Name, Arity]);
format_error_reason({unbound_var, Var}) ->
    io_lib:format("unbound variable ~s", [Var]);
format_error_reason(Other) ->
    io_lib:format("~p", [Other]).

log_line_error(File, Line, Msg) ->
    lpad_log:log_error("~s:~b: ~s~n", [File, Line, Msg]).

log_line_error(File, Line, Col, Msg) ->
    lpad_log:log_error("~s:~b:~b: ~s~n", [File, Line, Col, Msg]).

log_general_error(BannerTitle, Error) ->
    BannerMsg = lpad_log:format_banner(BannerTitle, "~p"),
    lpad_log:log_error(BannerMsg, [Error]).
