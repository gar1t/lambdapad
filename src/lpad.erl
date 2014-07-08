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

-module(lpad).

-export([run/0, run/1]).

-define(INDEX_MODULE, index).

run() ->
    {ok, Root} = file:get_cwd(),
    run(Root).

run(Root) ->
    handle_error(catch(run_impl(Root))).

run_impl(Root) ->
    lpad_session:init(Root),
    process_index(index_module(Root)).

handle_error({'EXIT', Err}) ->
    lpad_err:format(Err);
handle_error(ok) -> ok.

index_module(Root) ->
    compile_index(index_source(Root)).

index_source(Root) ->
    filename:join(Root, index_src_name()).

index_src_name() ->
    atom_to_list(?INDEX_MODULE) ++ ".erl".

compile_index(Src) ->
    CompileOpts =
        [export_all,
         return_errors, 
         binary,
         {i, lpad_include_dir()}],
    handle_index_compile(compile:file(Src, CompileOpts), Src).

lpad_include_dir() -> filename:join(app_dir(), "include").

app_dir() -> filename:dirname(ebin_dir()).

ebin_dir() -> filename:dirname(code:which(?MODULE)).

handle_index_compile({ok, Module, Bin}, Src) ->
    handle_index_load(code:load_binary(Module, Src, Bin));
handle_index_compile({error, Errors, Warnings}, Src) ->
    error({index_compile, Src, Errors, Warnings}).

handle_index_load({module, Module}) ->
    Module;
handle_index_load({error, Err}) ->
    error({index_load, Err}).

process_index(Index) ->
    Data = Index:data(),
    Site = Index:site(Data),
    create_site(Site, Index, Data).

create_site([M|Rest], Index, Data) ->
    call_site_handler(M, Index, Data),
    create_site(Rest, Index, Data);
create_site([], _Index, _Data) -> ok.

call_site_handler(F, Index, Data) when is_atom(F) ->
    Index:F(Data);
call_site_handler({M, F}, _Index, Data) ->
    M:F(Data);
call_site_handler(Other, _Index, _Data) ->
    error({site_handler, Other}).
