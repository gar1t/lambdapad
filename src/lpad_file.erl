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

-export([copy_file/2, write_file/2, create_dir/1, handle_generator_spec/2]).

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
%%% Create dir
%%%===================================================================

create_dir(Dir) ->
    Fake = filename:join(Dir, "."),
    ensure_dir(Fake).

%%%===================================================================
%%% Generator support
%%%===================================================================

handle_generator_spec({Target, {file, Source}}, Data) ->
    handle_file_spec(Target, Source, Data);
handle_generator_spec({Target, {files, Pattern}}, Data) ->
    handle_files_spec(Target, Pattern, Data);
handle_generator_spec({Target, {dir, Dir}}, Data) ->
    handle_dir_spec(Target, Dir, Data);
handle_generator_spec(_, Data) ->
    {continue, Data}.

%%%-------------------------------------------------------------------
%%% Single file
%%%-------------------------------------------------------------------

handle_file_spec(Target, Source, Data) ->
    AbsTarget = lpad_session:abs_path(Target),
    AbsSource = lpad_session:abs_path(Source),
    Targets = [{AbsTarget, [AbsSource], copy_file_gen(AbsSource, AbsTarget)}],
    {ok, Targets, Data}.

copy_file_gen(Src, Dest) ->
    fun() -> copy_file(Src, Dest) end.

%%%-------------------------------------------------------------------
%%% Files matching a pattern
%%%-------------------------------------------------------------------

handle_files_spec(Target, Pattern, Data) ->
    AbsTarget = abs_target_dir(Target),
    Sources = sources_for_pattern(Pattern),
    TargetSourcePairs = file_target_source_pairs(AbsTarget, Sources),
    Targets = [file_target(Pair) || Pair <- TargetSourcePairs],
    {ok, Targets, Data}.

abs_target_dir(Dir) ->
    lpad_session:abs_path(filename:dirname(Dir)).

sources_for_pattern(Pattern) ->
    filelib:wildcard(Pattern, lpad_session:root()).

file_target_source_pairs(TargetDir, Sources) ->
    [file_target_source_pair(TargetDir, Source) || Source <- Sources].

file_target_source_pair(TargetDir, Source) ->
    AbsSource = lpad_session:abs_path(Source),
    SourceName = filename:basename(Source),
    {filename:join(TargetDir, SourceName), AbsSource}.

file_target({Target, Source}) ->
    {Target, [Source], copy_file_gen(Source, Target)}.

%%%-------------------------------------------------------------------
%%% Single dir
%%%-------------------------------------------------------------------

handle_dir_spec(Target, Dir, Data) ->
    AbsTarget = lpad_session:abs_path(Target),
    AbsSource = lpad_session:abs_path(Dir),
    Targets =
        [file_or_dir_target(AbsTarget, AbsSource, Name)
         || Name <- find_all(AbsSource)],
    {ok, Targets, Data}.

find_all(Dir) ->
    filelib:wildcard("**", Dir).

file_or_dir_target(TargetDir, SourceDir, RelPath) ->
    AbsTarget = filename:join(TargetDir, RelPath),
    AbsSource = filename:join(SourceDir, RelPath),
    {AbsTarget, [AbsSource], file_or_dir_generator(AbsSource, AbsTarget)}.

file_or_dir_generator(Source, Target) ->
    file_or_dir_generator(file_type(Source), Source, Target).

file_type(Name) ->
    case filelib:is_file(Name) of
        true -> file;
        false -> dir
    end.

file_or_dir_generator(file, Source, Target) ->
    copy_file_gen(Source, Target);
file_or_dir_generator(dir, _Source, Target) ->
    create_dir_gen(Target).

create_dir_gen(Dir) ->
    fun() -> create_dir(Dir) end.
