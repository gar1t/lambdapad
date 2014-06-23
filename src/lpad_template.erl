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

-export([render/3]).

render(Template, Vars, Target) ->
    Compiled = compile(Template),
    Rendered = render(Compiled, Vars),
    write_file(Target, Rendered).

compile(Template) ->
    Mod = template_module(Template),
    handle_compile(erlydtl:compile(Template, Mod), Mod, Template).

template_module(Template) ->
    list_to_atom(Template).

handle_compile(ok, Mod, _Src) -> Mod;
handle_compile({error, Err}, _Mod, Src) ->
    error({template_compile, Src, Err}).

render(Mod, Vars) ->
    SafeVars = maps_to_proplists(Vars),
    handle_render(Mod:render(SafeVars), Mod).

maps_to_proplists(Map) when is_map(Map) ->
    [maps_to_proplists(Item) || Item <- maps:to_list(Map)];
maps_to_proplists(List) when is_list(List) ->
    [maps_to_proplists(Item) || Item <- List];
maps_to_proplists({Key, Value}) ->
    {maps_to_proplists(Key), maps_to_proplists(Value)};
maps_to_proplists(Other) ->
    Other.

handle_render({ok, Bin}, _Mod) -> Bin;
handle_render({error, Err}, Mod) ->
    {Src, _} = Mod:source(),
    error({template_render, Src, Err}).

write_file(File, Bin) ->
    ensure_dir(File),
    handle_write_file(file:write_file(File, Bin), File).

ensure_dir(File) ->
    handle_ensure_dir(filelib:ensure_dir(File), File).

handle_ensure_dir(ok, _File) -> ok;
handle_ensure_dir({error, Err}, File) ->
    error({ensure_dir, File, Err}).

handle_write_file(ok, _File) -> ok;
handle_write_file({error, Err}, File) ->
    error({write_file, File, Err}).
