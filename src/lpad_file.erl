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

-module(lpad_file).

-behavior(lpad_generator).

-export([copy_file/2, write_file/2, handle_generator_spec/2]).

%%%===================================================================
%%% Copy file
%%%===================================================================

copy_file(Src, Target) ->
    lpad_event:notify({file_copy, Src, Target}),
    ensure_dir(Target),
    handle_file_copy(file:copy(Src, Target), Src, Target).

ensure_dir(Target) ->
    handle_ensure_dir(filelib:ensure_dir(Target), Target).

handle_ensure_dir(ok, _Target) -> ok;
handle_ensure_dir({error, Err}, Target) ->
    error({ensure_dir, Target, Err}).

handle_file_copy({ok, _}, _Src, _Target) -> ok;
handle_file_copy({error, Err}, Src, Target) -> 
    error({file_copy, Src, Target, Err}).

%%%===================================================================
%%% Write file
%%%===================================================================

write_file(File, Bin) ->
    lpad_event:notify({file_create, File}),
    ensure_dir(File),
    handle_file_write(file:write_file(File, Bin), File).

handle_file_write(ok, _File) -> ok;
handle_file_write({error, Err}, File) ->
    error({file_write, File, Err}).

%%%===================================================================
%%% Generator support
%%%===================================================================

handle_generator_spec({Target, {file, Source}}, Data) ->
    handle_file_spec(Target, Source, Data);
handle_generator_spec({Target, {files, Pattern}}, Data) ->
    handle_files_spec(Target, Pattern, Data);
handle_generator_spec(_, Data) ->
    {continue, Data}.

handle_file_spec(Target, Source, Data) ->
    AbsTarget = lpad_session:abs_path(Target),
    AbsSource = lpad_session:abs_path(Source),
    Targets = [{AbsTarget, [AbsSource], copy_file_gen(AbsSource, AbsTarget)}],
    {ok, Targets, Data}.

copy_file_gen(Src, Dest) ->
    fun() -> copy_file(Src, Dest) end.

handle_files_spec(Target, Pattern, Data) ->
    TargetDir = abs_dir_without_pattern(Target, Pattern),
    TargetSourcePairs = apply_targets(find_sources(Pattern), TargetDir),
    Targets = [target_from_target_source(Pair) || Pair <- TargetSourcePairs],
    {ok, Targets, Data}.

abs_dir_without_pattern(Dir, Pattern) ->
    lpad_session:abs_path(strip_pattern(Dir, Pattern)).

strip_pattern(Dir, Pattern) ->
    DirParts = filename:split(Dir),
    case lists:last(DirParts) of
        Pattern -> filename:join(lists:droplast(DirParts));
        _ -> Dir
    end.

find_sources(Pattern) ->
    filelib:wildcard(Pattern, lpad_session:root()).

apply_targets(Sources, TargetDir) ->
    [target_source_pair(Source, TargetDir) || Source <- Sources].

target_source_pair(Source, TargetDir) ->
    {filename:join(TargetDir, Source), lpad_session:abs_path(Source)}.

target_from_target_source({Target, Source}) ->
    {Target, [Source], copy_file_gen(Source, Target)}.

