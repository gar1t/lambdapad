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

to_html(FileOrStr) ->
    to_html(filelib:is_file(FileOrStr), FileOrStr).

to_html(_IsFile=true, File) ->
    handle_mmd_result_html(mmd_cmd([File]));
to_html(_IsFile=false, Str) ->
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
      [os:getenv("LPAD_MMD_EXE"),
       os:find_executable("multimarkdown")]).

find_exe([false|Rest]) -> find_exe(Rest);
find_exe([Exe|_]) -> Exe;
find_exe([]) ->
    error("Cannot find multimarkdown - add to path or set "
          "LPAD_MMD_EXE environment variable").

redirect_mmd_cmd(Out, Args) ->
    lpad_cmd:run(redirect_exe(), [Out, mmd_exe()|Args]).

redirect_exe() ->
    filename:join([lpad:app_dir(), "bin", "lpad-exec-redirect"]).

%%%===================================================================
%%% Data loader support
%%%===================================================================

handle_data_spec({Name, {markdown, File}}, DSpec) ->
    {ok, lpad_util:load_file_data(Name, File, fun load_metadata/1, DSpec)};
handle_data_spec({markdown, File}, {'$root', Sources}) ->
    {ok, lpad_util:load_file_root_data(File, fun load_metadata/1, Sources)};
handle_data_spec(_, Data) ->
    {continue, Data}.
