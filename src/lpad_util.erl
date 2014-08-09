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

-module(lpad_util).

-export([printable_str/1,
         data_source_name/1,
         maps_to_proplists/1,
         load_file_data/4,
         load_file_root_data/2,
         add_data_source/2,
         maybe_add_data_source/2]).

printable_str(Str) ->
    [printable_char(Ch) || Ch <- Str].

printable_char(Ch) when Ch >= 8, Ch =< 255 -> Ch;
printable_char(_) -> 168.

data_source_name(AttrName) ->
    {src, AttrName}.

maps_to_proplists(Map) when is_map(Map) ->
    [maps_to_proplists(Item) || Item <- maps:to_list(Map)];
maps_to_proplists(List) when is_list(List) ->
    [maps_to_proplists(Item) || Item <- List];
maps_to_proplists({Key, Value}) ->
    {maps_to_proplists(Key), maps_to_proplists(Value)};
maps_to_proplists(Other) ->
    Other.

load_file_data(Name, FileOrPattern, LoadFun, Data) ->
    AbsFileOrPattern = lpad_session:abs_path(FileOrPattern),
    load_resolved_file_data(
      resolve_files(AbsFileOrPattern), Name, LoadFun, Data).

resolve_files(FileOrPattern) ->
    resolve_files(filelib:is_file(FileOrPattern), FileOrPattern).

resolve_files(_IsFile=true, File) ->
    {file, File};
resolve_files(_IsFile=false, Pattern) ->
    {pattern, filelib:wildcard(Pattern)}.

load_resolved_file_data({file, File}, Name, LoadFun, Data) ->
    Value = LoadFun(File),
    [{Name, maybe_add_data_source(File, Value)}|Data];
load_resolved_file_data({pattern, Files}, Name, LoadFun, Data) ->
    [{Name, load_resolved_files_data(Files, LoadFun, [])}|Data].

load_resolved_files_data([File|Rest], LoadFun, Data) ->
    Name = file_name(File),
    NextData = load_resolved_file_data({file, File}, Name, LoadFun, Data),
    load_resolved_files_data(Rest, LoadFun, NextData);
load_resolved_files_data([], _LoadFun, Data) ->
    Data.

file_name(File) ->
    filename:basename(File, filename:extension(File)).

load_file_root_data(File, LoadFun) ->
    AbsFile = lpad_session:abs_path(File),
    Value = LoadFun(AbsFile),
    maybe_add_data_source(AbsFile, Value).

maybe_add_data_source(Source, [{_, _}|_]=Proplist) ->
    add_data_source(Source, Proplist);
maybe_add_data_source(_Source, Value) ->
    Value.

add_data_source(Source, Proplist) ->
    [{'__file__', Source}|Proplist].
