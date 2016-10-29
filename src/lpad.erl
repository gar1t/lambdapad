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

-export([run/1, run/2, debug/1]).

-export([app_dir/0]).

-include_lib("kernel/include/file.hrl").

-define(INDEX_MODULE, index).

run(Args) ->
    run(cwd(), Args).

debug(Args) ->
    debugger:start(local),
    debugger:quick(lpad, run, [Args]).

cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

run(Root, Args) ->
    handle_error(catch(run_impl(Root, Args))).

run_impl(Root, Args) ->
    lpad_session:init(Root),
    IndexMod = index_module(Root),
    lpad_trace:init(),
    process_index(IndexMod, Args).

handle_error({'EXIT', Err}) ->
    lpad_event:notify({exit, Err}),
    error;
handle_error(ok) ->
    ok.

index_module(Root) ->
    compile_index(index_source(Root)).

index_source(Root) ->
    filename:join(Root, index_src_name()).

index_src_name() ->
    atom_to_list(?INDEX_MODULE) ++ ".erl".

compile_index(Src) ->
    CompileOpts = index_compile_options(),
    handle_index_compile(compile:file(Src, CompileOpts), Src).

index_compile_options() ->
    BaseOpts = [return_errors, binary],
    maybe_export_all_option(BaseOpts).

maybe_export_all_option(BaseOpts) ->
    maybe_export_all_option(not require_index_export(), BaseOpts).

require_index_export() ->
    case os:getenv("LPAD_INDEX_REQUIRE_EXPORT") of
        false -> false;
        "" -> false;
        _ -> true
    end.

maybe_export_all_option(true, BaseOpts) ->
    [export_all|BaseOpts];
maybe_export_all_option(false, BaseOpts) ->
    BaseOpts.

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

process_index(Index, Args) ->
    DLs = init_data_loaders(Index),
    DSpecs = data_specs(Index, Args),
    {Data, Sources} = data(DSpecs, DLs),
    lpad_event:notify({data_loaded, Data}),
    Generators = init_generators(Index),
    GSpecs = generator_specs(Index, Data),
    lpad_event:notify({generators_loaded, GSpecs}),
    Targets = generator_targets(GSpecs, Data, Generators),
    generate(Targets, Sources).

init_generators(_Index) ->
    [lpad_template,
     lpad_file].

init_data_loaders(_Index) ->
    [lpad_eterm,
     lpad_json,
     lpad_markdown,
     lpad_guess].

data_specs(Index, Args) ->
    try_call_index_function(Index, data, [Args]).

try_call_index_function(M, F, A) ->
    Arity = length(A),
    case erlang:function_exported(M, F, Arity) of
        true -> M:F(A);
        false -> error({index_function_not_expored, {M, F, Arity}})
    end.

data(DSpecs, DLs) ->
    data_impl(plist:convert_maps(DSpecs), DLs).

data_impl([{_, _}|_]=DSpecs, DLs) ->
    usort_data_sources(acc_data(DSpecs, DLs, init_data_state()));
data_impl(DSpec, DLs) ->
    usort_data_sources(apply_data_loader(DLs, DSpec, init_root_data_state())).

init_data_state() -> {[], [index_source()]}.

init_root_data_state() -> {'$root', [index_source()]}.

usort_data_sources({Data, Sources}) ->
    {Data, lists:usort(Sources)}.

index_source() ->
    index_source(lpad_session:root()).

acc_data([DSpec|Rest], DLs, DState) ->
    acc_data(Rest, DLs, apply_data_loader(DLs, DSpec, DState));
acc_data([], _DLs, DState) ->
    DState.

apply_data_loader(DLs, {Name, {apply, FunSpec, DSpec}}, DState) ->
    handle_apply(FunSpec, DSpec, Name, DLs, DState);
apply_data_loader(DLs, {apply, FunSpec, DSpec}, {'$root', Sources}) ->
    handle_apply_root(FunSpec, DSpec, DLs, Sources);
apply_data_loader([DL|Rest], DSpec, DState) ->
    handle_data_loader_result(
      DL:handle_data_spec(DSpec, DState),
      Rest, DSpec);
apply_data_loader([], {Name, Value}, {Data, Sources}) ->
    {[{Name, Value}|Data], Sources};
apply_data_loader([], Term, {'$root', Sources}) ->
    {Term, Sources};
apply_data_loader([], DSpec, _Data) ->
    error({unhandled_data_spec, DSpec, _Data}).

handle_data_loader_result({continue, DState}, Rest, DSpec) ->
    apply_data_loader(Rest, DSpec, DState);
handle_data_loader_result({ok, DState}, _Rest, _DSpec) ->
    DState;
handle_data_loader_result({stop, Reason}, _Rest, DSpec) ->
    error({data_loader_stop, Reason, DSpec}).

handle_apply(FunSpec, DSpec, Name, DLs, {Data, Sources}) ->
    {ApplyData, ApplySources} = data(DSpec, DLs),
    Value = apply_funspec(FunSpec, ApplyData),
    {[{Name, Value}|Data], acc_items(ApplySources, Sources)}.

handle_apply_root(FunSpec, DSpec, DLs, Sources) ->
    {ApplyData, ApplySources} = data(DSpec, DLs),
    Value = apply_funspec(FunSpec, ApplyData),
    {Value, acc_items(ApplySources, Sources)}.

apply_funspec(Fun, Data) when is_function(Fun) ->
    Fun(Data);
apply_funspec([FunSpec|Rest], Data) ->
    apply_funspec(Rest, apply_funspec(FunSpec, Data));
apply_funspec([], Data) ->
    Data.

generator_specs(Index, Data) ->
    Site = try_call_index_function(Index, site, [Data]),
    plist:convert_maps(Site).

generator_targets(GSpecs, Data, Gs) ->
    acc_targets(GSpecs, Data, Gs, []).

acc_targets([GSpec|Rest], Data, Gs, Acc) ->
    Targets = apply_generator(Gs, GSpec, Data),
    acc_targets(Rest, Data, Gs, acc_items(Targets, Acc));
acc_targets([], _Data, _Gs, Acc) ->
    lists:reverse(Acc).

apply_generator([G|Rest], GSpec, Data) ->
    handle_generator_result(
      G:handle_generator_spec(GSpec, Data),
      Rest, GSpec, Data);
apply_generator([], GSpec, _Data) ->
    error({unhandled_generator_spec, GSpec}).

handle_generator_result(continue, Rest, GSpec, Data) ->
    apply_generator(Rest, GSpec, Data);
handle_generator_result({ok, Targets}, _Rest, _GSpec, _Data) ->
    Targets;
handle_generator_result({stop, Reason}, _Rest, GSpec, _Data) ->
    error({generator_stop, Reason, GSpec}).

acc_items([Item|Rest], Acc) ->
    acc_items(Rest, [Item|Acc]);
acc_items([], Acc) ->
    Acc.

generate([{Target, TargetSources, Generator}|Rest], DataSources) ->
    AllSources = resolve_sources(TargetSources, DataSources),
    maybe_generate_target(target_stale(Target, AllSources), Generator),
    generate(Rest, DataSources);
generate([], _DataSources) ->
    ok.

resolve_sources(TargetSources, DataSources) ->
    acc_resolved_sources(TargetSources, DataSources, []).

acc_resolved_sources(['$data'|Rest], DataSources, Acc) ->
    acc_resolved_sources(Rest, [], acc_items(DataSources, Acc));
acc_resolved_sources([Source|Rest], DataSources, Acc) ->
    acc_resolved_sources(Rest, DataSources, [Source|Acc]);
acc_resolved_sources([], _DataSources, Acc) -> Acc.

target_stale(Target, Sources) ->
    any_source_newer(modified(Target), map_modified(Sources)).

-define(NO_MTIME, {{0,0,0},{0,0,0}}).

modified(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime=Modified}} -> Modified;
        _ -> ?NO_MTIME
    end.

map_modified(Files) ->
    [modified(File) || File <- Files].

any_source_newer(?NO_MTIME, _Sources) -> true;
any_source_newer(_Target, [force_modified|_]) -> true;
any_source_newer(Target, [Source|_]) when Source > Target -> true;
any_source_newer(Target, [_|Rest]) -> any_source_newer(Target, Rest);
any_source_newer(_Target, []) -> false.

maybe_generate_target(true, Generator) -> Generator();
maybe_generate_target(false, _Generator) -> ok.
