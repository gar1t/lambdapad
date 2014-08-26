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

-module(lpad_guess).

-behavior(lpad_data_loader).

-export([handle_data_spec/2]).

handle_data_spec(DSpec, DState) ->
    maybe_apply_spec(guess_handler(DSpec), DState).

guess_handler({Name, MaybeFile}) ->
    maybe_add_name(maybe_guess_handler_for_file(MaybeFile), Name);
guess_handler(MaybeFile) ->
    maybe_guess_handler_for_file(MaybeFile).

maybe_guess_handler_for_file(MaybeFile) ->
    guess_handler_for_file(lpad_util:try_abs_path(MaybeFile)).

guess_handler_for_file({ok, File}) ->
    Ext = filename:extension(File),
    guess_handler_for_ext(Ext, File);
guess_handler_for_file(error) ->
    unknown.

guess_handler_for_ext(".config",   File) -> {lpad_eterm,    {eterm, File}};
guess_handler_for_ext(".json",     File) -> {lpad_json,     {json, File}};
guess_handler_for_ext(".md",       File) -> {lpad_markdown, {markdown, File}};
guess_handler_for_ext(".markdown", File) -> {lpad_markdown, {markdown, File}};
guess_handler_for_ext(_,          _File) -> unknown.

maybe_add_name(unknown, _Name) -> unknown;
maybe_add_name({Handler, DSpec}, Name) -> {Handler, {Name, DSpec}}.

maybe_apply_spec({Handler, DSpec}, DState) ->
    Handler:handle_data_spec(DSpec, DState);
maybe_apply_spec(unknown, DState) ->
    {continue, DState}.
