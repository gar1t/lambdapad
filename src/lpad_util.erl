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
         load_file_data/4,
         load_file_root_data/3]).

%%%-------------------------------------------------------------------
%%% Printable string
%%%-------------------------------------------------------------------

printable_str(Str) ->
    [printable_char(Ch) || Ch <- Str].

printable_char(Ch) when Ch >= 8, Ch =< 255 -> Ch;
printable_char(_) -> 168.

data_source_name(AttrName) ->
    {src, AttrName}.

%%%-------------------------------------------------------------------
%%% Load file data
%%%-------------------------------------------------------------------

load_file_data(Name, FileOrPattern, LoadFun, DState) ->
    AbsFileOrPattern = lpad_session:abs_path(FileOrPattern),
    Files = resolve_files(AbsFileOrPattern),
    load_resolved_file_data(Files, Name, LoadFun, DState).

resolve_files(FileOrPattern) ->
    resolve_files(filelib:is_file(FileOrPattern), FileOrPattern).

resolve_files(_IsFile=true, File) ->
    {file, File};
resolve_files(_IsFile=false, Pattern) ->
    {pattern, filelib:wildcard(Pattern)}.

load_resolved_file_data({file, File}, Name, LoadFun, {Data, Sources}) ->
    Item = load_file(File, LoadFun),
    {[{Name, Item}|Data], [File|Sources]};
load_resolved_file_data({pattern, Files}, Name, LoadFun, {Data, Sources}) ->
    Items = load_files(Files, LoadFun),
    {[{Name, Items}|Data], lists:append(Files, Sources)}.

load_file(File, LoadFun) ->
    maybe_add_file_source(LoadFun(File), File).

maybe_add_file_source([{_, _}|_]=Value, File) ->
    [{'__file__', File}|Value];
maybe_add_file_source([], File) ->
    [{'__file__', File}];
maybe_add_file_source(Value, _File) ->
    Value.

load_files(Files, LoadFun) ->
    acc_file_data(Files, LoadFun, []).

acc_file_data([File|Rest], LoadFun, Acc) ->
    acc_file_data(Rest, LoadFun, [load_file(File, LoadFun)|Acc]);
acc_file_data([], _LoadFun, Acc) ->
    Acc.

%%%-------------------------------------------------------------------
%%% Load root file data
%%%-------------------------------------------------------------------

load_file_root_data(File, LoadFun, Sources) ->
    AbsFile = lpad_session:abs_path(File),
    {load_file(AbsFile, LoadFun), [AbsFile|Sources]}.
