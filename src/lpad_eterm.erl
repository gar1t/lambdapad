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

-module(lpad_eterm).

-export([load/1, handle_data_spec/2]).

%%%===================================================================
%%% Load
%%%===================================================================

load(File) ->
    handle_consult_eterm(file:consult(File), File).

handle_consult_eterm({ok, [Term]}, _Source) ->
    lpad_util:maps_to_proplists(Term);
handle_consult_eterm({ok, _}, Source) ->
    error({eterm_source, Source, invalid_term});
handle_consult_eterm({error, Err}, Source) ->
    error({eterm_source, Source, Err}).

%%%===================================================================
%%% Data loader support
%%%===================================================================

handle_data_spec({Name, {eterm, TermOrFile}}, DState) ->
    {ok, maybe_load_file(is_file(TermOrFile), Name, TermOrFile, DState)};
handle_data_spec({eterm, TermOrFile}, {'$root', Sources}) ->
    {ok, maybe_load_file_root(is_file(TermOrFile), TermOrFile, Sources)};
handle_data_spec(_, Data) ->
    {continue, Data}.

is_file(TermOrFile) -> is_string(TermOrFile).

is_string(L) when is_list(L) ->
    lists:all(fun erlang:is_integer/1, L);
is_string(_) -> false.

maybe_load_file(_File=true, Name, File, DState) ->
    lpad_util:load_file_data(Name, File, fun load/1, DState);
maybe_load_file(_File=false, Name, Value, {Data, Sources}) ->
    {[{Name, Value}|Data], Sources}.

maybe_load_file_root(_File=true, File, Sources) ->
    lpad_util:load_file_root_data(File, fun load/1, Sources);
maybe_load_file_root(_File=false, Value, Sources) ->
    {Value, Sources}.
