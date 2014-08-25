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

-module(lpad_json).

-behavior(lpad_data_loader).

-export([load/1, handle_data_spec/2]).

%%%===================================================================
%%% Load
%%%===================================================================

load(File) ->
    handle_json_file(file:read_file(File), File).

handle_json_file({ok, Bin}, _File) ->
    structs_to_proplists(jiffy:decode(Bin));
handle_json_file({error, Err}, File) ->
    error({read_file, File, Err}).

structs_to_proplists({Proplist}) ->
    [{Name, structs_to_proplists(Val)} || {Name, Val} <- Proplist];
structs_to_proplists(Other) ->
    Other.

%%%===================================================================
%%% Data loader support
%%%===================================================================

handle_data_spec({Name, {json, File}}, DState) ->
    {ok, lpad_util:load_file_data(Name, File, fun load/1, DState)};
handle_data_spec({json, File}, {'$root', Sources}) ->
    {ok, lpad_util:load_file_root_data(File, fun load/1, Sources)};
handle_data_spec(_, DState) ->
    {continue, DState}.
