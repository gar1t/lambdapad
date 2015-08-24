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

-module(lpad_template).

-behavior(lpad_generator).

-export([render/3, render_string/2]).

-export([handle_generator_spec/2]).

%%%===================================================================
%%% Render
%%%===================================================================

render(Template, Vars, Target) ->
    Compiled = compile_file(Template),
    Rendered = render_compiled(Compiled, Vars),
    write_file(Target, Rendered).

compile_file(Template) ->
    Mod = template_module(Template),
    Opts =
        [{custom_filters_modules, [index, lpad_template_filters]},
         {custom_tags_modules, [index]}],
    handle_compile(erlydtl:compile(Template, Mod, Opts), Mod, Template).

template_module(Template) ->
    list_to_atom(Template).

handle_compile(ok, Mod, _Src) -> Mod;
handle_compile({ok, Mod}, Mod, _Str) -> Mod;
handle_compile({error, Err}, _Mod, Src) ->
    error({template_compile, Src, Err}).

render_compiled(Mod, Vars) ->
    handle_render(Mod:render(Vars), Mod).

handle_render({ok, Bin}, _Mod) -> Bin;
handle_render({error, Err}, Mod) ->
    {Src, _} = Mod:source(),
    error({template_render, Src, Err}).

write_file(File, Bin) ->
    ensure_dir(File),
    lpad_event:notify({file_create, File}),
    handle_write_file(file:write_file(File, Bin), File).

ensure_dir(File) ->
    handle_ensure_dir(filelib:ensure_dir(File), File).

handle_ensure_dir(ok, _File) -> ok;
handle_ensure_dir({error, Err}, File) ->
    error({ensure_dir, File, Err}).

handle_write_file(ok, _File) -> ok;
handle_write_file({error, Err}, File) ->
    error({write_file, File, Err}).

%%%===================================================================
%%% Render string
%%%===================================================================

render_string(Str, Vars) ->
    Compiled = compile_str(unicode:characters_to_binary(Str)),
    iolist_to_binary(render_compiled(Compiled, Vars)).

compile_str(Str) ->
    Template = unicode:characters_to_binary(Str),
    Mod = str_module(Str),
    Opts = [{custom_filters_modules, [index, lpad_template_filters]}],
    handle_compile(erlydtl:compile(Template, Mod, Opts), Mod, Str).

str_module(Str) ->
    list_to_atom("string-" ++ integer_to_list(erlang:phash2(Str))).

%%%===================================================================
%%% Generator support
%%%===================================================================

handle_generator_spec({Target, {template, Template}}, Data) ->
    handle_template(Template, Data, Target);
handle_generator_spec({Target, {template, Template, Vars}}, Data) ->
    handle_template_with_vars(Template, Vars, Data, Target);
handle_generator_spec({Target, {template_map, Template, List}}, Data) ->
    handle_map_template(Template, List, Data, Target);
handle_generator_spec({Target, {template_map, Template, List, Vars}}, Data) ->
    handle_map_template_with_vars(Template, List, Vars, Data, Target);
handle_generator_spec({Target, {string, Str}}, Data) ->
    handle_string(Str, Data, Target);
handle_generator_spec({Target, Str}, Data) when is_binary(Str) ->
    handle_string(Str, Data, Target);
handle_generator_spec({Target, MaybeStr}, Data) when is_list(MaybeStr) ->
    handle_template_or_string(apply_tag(MaybeStr), Data, Target);
handle_generator_spec(_, _Data) ->
    continue.

%%%-------------------------------------------------------------------
%%% Template
%%%-------------------------------------------------------------------

handle_template(Template, Data, Target) ->
    {ok, [generator_for_template(Template, Data, Target)]}.

generator_for_template(Template, Data, Target) ->
    AbsTarget = abs_target(Target, Data),
    AbsTemplate = lpad_session:abs_path(Template),
    Generator = fun() -> render(AbsTemplate, Data, AbsTarget) end,
    Sources = [AbsTemplate, '$data'],
    {AbsTarget, Sources, Generator}.

abs_target(Target, Data) ->
    render_string(lpad_session:abs_path(Target), Data).

%%%-------------------------------------------------------------------
%%% Template with vars
%%%-------------------------------------------------------------------

handle_template_with_vars(Template, Vars, Data, Target) ->
    handle_template(Template, extend_data(Data, Vars), Target).

extend_data(Data, Extra) ->
    lists:append(plist:convert_maps(Extra), Data).

%%%-------------------------------------------------------------------
%%% Map template
%%%-------------------------------------------------------------------

handle_map_template(Template, {ContextName, List}, Data, Target) ->
    acc_generators(List, ContextName, Template, Data, Target, []);
handle_map_template(Template, List, Data, Target) ->
    acc_generators(List, item, Template, Data, Target, []).

acc_generators([Context|Rest], ContextName, Template, Data, Target, Acc) ->
    ExtendedData = [{ContextName, Context}|Data],
    ContextSrc = context_source(Context),
    Gen = generator_for_template(Template, ExtendedData, Target, ContextSrc),
    acc_generators(Rest, ContextName, Template, Data, Target, [Gen|Acc]);
acc_generators([], _ContextName, _Template, _Data, _Target, Acc) ->
    {ok, Acc}.

context_source(Context) ->
    proplists:get_value('__file__', Context).

generator_for_template(Template, Data, Target, undefined) ->
    generator_for_template(Template, Data, Target);
generator_for_template(Template, Data, Target, ExtraSource) ->
    {AbsTarget, Sources, Generator} =
        generator_for_template(Template, Data, Target),
    {AbsTarget, [ExtraSource|Sources], Generator}.

%%%-------------------------------------------------------------------
%%% Map template with vars
%%%-------------------------------------------------------------------

handle_map_template_with_vars(Template, List, Vars, Data, Target) ->
    handle_map_template(Template, List, extend_data(Data, Vars), Target).

%%%-------------------------------------------------------------------
%%% String
%%%-------------------------------------------------------------------

handle_string(Str, Data, Target) ->
    AbsTarget = abs_target(Target, Data),
    Value = render_string(Str, Data),
    Generator = fun() -> lpad_file:write_file(AbsTarget, Value) end,
    {ok, [{AbsTarget, ['$data'], Generator}]}.

%%%-------------------------------------------------------------------
%%% String vs template decision
%%%-------------------------------------------------------------------

apply_tag(Term) ->
    tag_template_or_string(lpad_util:try_abs_path(Term), Term).

tag_template_or_string({ok, File}, _) -> {template, File};
tag_template_or_string(error, Str)    -> {string, Str}.

handle_template_or_string({template, Template}, Data, Target) ->
    handle_template(Template, Data, Target);
handle_template_or_string({string, Str}, Data, Target) ->
    handle_string(Str, Data, Target);
handle_template_or_string(error, _Data, _Target) ->
    continue.
