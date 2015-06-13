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

-module(lpad_markdown).

-behavior(lpad_data_loader).

-export([load_metadata/1, to_html/1, handle_data_spec/2]).

%%%===================================================================
%%% Load
%%%===================================================================

load_metadata(File) ->
    acc_key_values(keys(File), File, []).

keys(File) ->
    mmd_result_to_keys(mmd_cmd(["-m", File])).

mmd_result_to_keys({0, ""}) ->
    [];
mmd_result_to_keys({0, Out}) ->
    re:split(Out, "\n", [{return, list}, trim]);
mmd_result_to_keys({N, Err}) ->
    error({markdown_metadata, {N, Err}}).

acc_key_values([Key|Rest], File, Acc) ->
    acc_key_values(Rest, File, try_metadata(File, Key, Acc));
acc_key_values([], _File, Acc) -> Acc.

try_metadata(File, Key, Acc) ->
    acc_mmd_result_metadata(mmd_cmd(["-e", Key, File]), Key, Acc).

acc_mmd_result_metadata({0, Out}, Key, Acc) ->
    [{Key, strip_trailing_lf(Out)}|Acc];
acc_mmd_result_metadata({N, Err}, _Key, _Acc) ->
    error({mmd_metadata, {N, Err}}).

strip_trailing_lf(Str) ->
    re:replace(Str, "\n$", "", [{return, list}]).

%%%===================================================================
%%% Convert to HTML
%%%===================================================================

to_html(Term) ->
    to_html_impl(lpad_util:file_or_string(Term)).

to_html_impl({file, File}) ->
    handle_mmd_result_html(mmd_cmd(["-s", File]));
to_html_impl({string, Str}) ->
    handle_mmd_result_html(redirect_mmd_cmd(Str, [])).

handle_mmd_result_html({0, Out}) -> Out;
handle_mmd_result_html({N, Err}) ->
    error({mdd_html, {N, Err}}).

%%%===================================================================
%%% Cmd Support
%%%===================================================================

mmd_cmd(Args) ->
    lpad_cmd:run(mmd_exe(), Args).

mmd_exe() ->
    find_exe(
      [fun local_mmd_exe/0,
       fun() -> os:getenv("LPAD_MMD_EXE") end,
       fun() -> os:find_executable("multimarkdown") end]).

local_mmd_exe() ->
    Path = local_mmd_path(),
    maybe_path(filelib:is_regular(Path), Path).

local_mmd_path() ->
    filename:join([lpad:app_dir(), "deps", "mmd", "multimarkdown"]).

maybe_path(true, Path) -> Path;
maybe_path(false, _Path) -> false.

find_exe([Find|Rest]) ->
    handle_exe_find(Find(), Rest);
find_exe([]) ->
    error("Cannot find multimarkdown - add to path or set "
          "LPAD_MMD_EXE environment variable").

handle_exe_find(false, Rest) ->
    find_exe(Rest);
handle_exe_find(Exe, _Rest) ->
    Exe.

redirect_mmd_cmd(Out, Args) ->
    lpad_cmd:run(redirect_exe(), [Out, mmd_exe()|Args]).

redirect_exe() ->
    filename:join([lpad:app_dir(), "bin", "lpad-exec-redirect"]).

%%%===================================================================
%%% Data loader support
%%%===================================================================

handle_data_spec({Name, {markdown, File}}, DState) ->
    {ok, lpad_util:load_file_data(Name, File, fun load_file/1, DState)};
handle_data_spec({markdown, File}, {'$root', Sources}) ->
    {ok, lpad_util:load_file_root_data(File, fun load_file/1, Sources)};
handle_data_spec(_, DState) ->
    {continue, DState}.

load_file(File) ->
    add_file(load_metadata(File), File).

add_file(Metadata, File) ->
    [{'__file__', File}|Metadata].
