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

-module(lpad_imports).

-export([page/3,
         dirs/2,
         dir/2,
         files/2,
         file/2,
         eterm/1, markdown/1, json/1,
         ls/1]).

%%%===================================================================
%%% Site output types
%%%===================================================================

%%%-------------------------------------------------------------------
%%% page - generates a page using a template
%%%-------------------------------------------------------------------

page(Target, Template, Vars) ->
    AbsTarget = page_target(Target, Vars),
    AbsTemplate = abs_file(Template),
    lpad_log:event({create_file, AbsTarget}),
    lpad_template:render(AbsTemplate, Vars, AbsTarget).

page_target(Target, Vars) ->
    lpad_template:resolve_refs(abs_file(Target), Vars).

dirs(Pattern, Target) ->
    AbsPattern = abs_file(Pattern),
    AbsTarget = abs_file(Target),
    copy_dirs(match_dirs(AbsPattern), AbsTarget).

match_dirs(Pattern) ->
    filter_dirs(filelib:wildcard(Pattern)).

filter_dirs(Files) ->
    lists:filter(fun filelib:is_dir/1, Files).

copy_dirs([Dir|Rest], Target) ->
    handle_cp(cp_archive(Dir, Target), Dir, Target),
    copy_dirs(Rest, Target);
copy_dirs([], _Target) -> ok.

cp_archive(Dir, Target) ->
    lpad_log:event({copy_dir, Dir, Target}),
    Cmd = ["mkdir -p '", Target, "' && cp -a '", Dir, "' '", Target, "'"],
    os:cmd(Cmd).

handle_cp("", _Src, _Target) -> ok;
handle_cp(Err, Src, Target) ->
    error({cp, Src, Target, lpad_util:printable_str(Err)}).

dir(Dir, Target) ->
    AbsDir = abs_file(Dir),
    AbsTarget = abs_file(Target),
    copy_dirs([AbsDir], AbsTarget).

files(Pattern, Target) ->
    AbsPattern = abs_file(Pattern),
    AbsTarget = abs_file(Target),
    copy_files(match_files(AbsPattern), AbsTarget).

match_files(Pattern) ->
    AbsPattern = abs_file(Pattern),
    filter_files(filelib:wildcard(AbsPattern)).

filter_files(Files) ->
    lists:filter(fun filelib:is_file/1, Files).

copy_files([File|Rest], Target) ->
    handle_cp(cp(File, Target), File, Target),
    copy_files(Rest, Target);
copy_files([], _Target) -> ok.

cp(Src, Target) ->
    lpad_log:event({copy_file, Src, Target}),
    os:cmd("cp '" ++ Src ++ "' '" ++ Target ++ "'").

file(File, Target) ->
    AbsFile = abs_file(File),
    AbsTarget = abs_file(Target),
    copy_files([AbsFile], AbsTarget).

%%%===================================================================
%%% Data types
%%%===================================================================

eterm(Source) ->
    lpad_eterm:load(abs_file(Source)).

markdown(Source) ->
    {Headers, HTML, Raw} = lpad_markdown:load(abs_file(Source)),
    markdown_map(Headers, Raw, HTML).

markdown_map(Headers, Raw, HTML) ->
    Headers#{'HTML' => HTML, 'RAW' => Raw}.

json(Source) ->
    lpad_json:load(abs_file(Source)).

abs_file(Rel) ->
    filename:join(lpad_session:root(), Rel).

%%%===================================================================
%%% Misc
%%%===================================================================

ls(Pattern) ->
    AbsPattern = abs_file(Pattern),
    fillib:wildcard(AbsPattern).
