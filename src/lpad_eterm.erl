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
    plist:convert_maps(Term);
handle_consult_eterm({ok, _}, Source) ->
    error({eterm_source, Source, invalid_term});
handle_consult_eterm({error, Err}, Source) ->
    error({eterm_source, Source, Err}).

%%%===================================================================
%%% Data loader support
%%%===================================================================

handle_data_spec({Name, {eterm, Eterm}}, DState) ->
    {ok, handle_eterm(apply_eterm_type(Eterm), Name, DState)};
handle_data_spec({eterm, Eterm}, {'$root', Sources}) ->
    {ok, handle_eterm_root(apply_eterm_type(Eterm), Sources)};
handle_data_spec(_, DState) ->
    {continue, DState}.

apply_eterm_type(Eterm) ->
    eterm_type(lpad_util:try_abs_path(Eterm), Eterm).

eterm_type({ok, AbsPath}, _) -> {file, AbsPath};
eterm_type(error, Term) -> {term, Term}.

handle_eterm({file, File}, Name, DState) ->
    lpad_util:load_file_data(Name, File, fun load/1, DState);
handle_eterm({term, Value}, Name, {Data, Sources}) ->
    {[{Name, Value}|Data], Sources}.

handle_eterm_root({file, File}, Sources) ->
    lpad_util:load_file_root_data(File, fun load/1, Sources);
handle_eterm_root({term, Value}, Sources) ->
    {Value, Sources}.
