%%% Copyright 2014 Garrett Smith <g@rre.tt>
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

-module(lpad_event).

-export([notify/1]).

notify({file_create, File}) ->
    log_info("Creating ~s~n", [File]);
notify({file_copy, _Src, Dest}) ->
    log_info("Creating ~s~n", [Dest]);
notify({data_loaded, Data}) ->
    maybe_log_info(
      env_defined(["LPAD_LOG_ALL", "LPAD_LOG_DATA"]),
      banner("Data"), [Data]);
notify({generators_loaded, Generators}) ->
    maybe_log_info(
      env_defined(["LPAD_LOG_ALL", "LPAD_LOG_GENERATORS"]),
      banner("Generators"), [Generators]);
notify({exit, Err}) ->
    handle_error(Err);
notify(Other) ->
    log_info(banner("UNKNOWN"), [Other]).

banner(Name) -> "=== " ++ Name ++ " ===~n~p~n~n".

log_info(Msg, Data) ->
    io:format(Msg, Data).

log_error(Msg, Data) ->
    io:format(standard_error, Msg, Data).

maybe_log_info(true, Msg, Data) ->
    log_info(Msg, Data);
maybe_log_info(false, _Msg, _Data) ->
    ok.

env_defined(Names) ->
    lists:any(fun(Name) -> os:getenv(Name) /= false end, Names).

handle_error({{template_compile, _, Error}, _}) ->
    handle_template_compile_error(Error);
handle_error({{index_compile, _, Error, _}, _}) ->
    handle_index_compile_error(Error);
handle_error({{eterm_source, File, Error}, _}) ->
    handle_eterm_source_error(File, Error);
%% handle_error({Reason, [{_, _, _, [{file, File}, {line, Line}]}|_]}) ->
%%     handle_general_error(Reason, File, Line);
%% handle_error({{_, File, {Line, erl_parse, Msg}}, _}) ->
%%     log_line_error(File, Line, Msg);
handle_error(Other) ->
    log_error(banner("ERROR"), [Other]).

handle_template_compile_error({T, [{Line, erlydtl_scanner, Msg}]}) ->
    log_line_error(T, Line, Msg);
handle_template_compile_error({T, [{{Line, Col}, erlydtl_parser, Msg}]}) ->
    log_line_error(T, Line, Col, Msg);
handle_template_compile_error(Other) ->
    log_error(banner("TEMPLATE COMPILE ERROR"), [Other]).

handle_index_compile_error(Errors) when is_list(Errors) ->
    lists:foreach(fun log_index_error/1, Errors);
handle_index_compile_error([{File, Errors}]) ->
    lists:foreach(fun(Err) -> log_index_error(File, Err) end, Errors);
handle_index_compile_error(Error) ->
    log_error(banner("INDEX COMPILE ERROR"), [Error]).

handle_eterm_source_error(File, {Line, erl_parse, Msg}) ->
    log_line_error(File, Line, Msg).

log_index_error({File, Errors}) ->
    lists:foreach(fun(Err) -> log_index_error(File, Err) end, Errors).

log_index_error(File, {Line, erl_parse, Msg}) ->
    log_line_error(File, Line, Msg);
log_index_error(File, {Line, erl_lint, Reason}) ->
    log_line_error(File, Line, format_error_reason(Reason));
log_index_error(File, Other) ->
    log_error(banner("INDEX ERROR"), [{File, Other}]).

handle_general_error(Reason, File, Line) ->
    log_line_error(File, Line, format_error_reason(Reason)).

format_error_reason({undefined_function, {Name, Arity}}) ->
    io_lib:format("undefined function ~s/~b", [Name, Arity]);
format_error_reason({unbound_var, Var}) ->
    io_lib:format("unbound variable ~s", [Var]);
format_error_reason(Other) ->
    io_lib:format("~p", [Other]).

log_line_error(File, Line, Msg) ->
    log_error("~s:~b: ~s~n", [File, Line, Msg]).

log_line_error(File, Line, Col, Msg) ->
    log_error("~s:~b:~b: ~s~n", [File, Line, Col, Msg]).
