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

-module(lpad_log).

-export([event/1]).

event({create_file, File}) ->
    io:format("Creating ~s~n", [File]);
event({copy_dir, Src, Dest}) ->
    io:format("Copying dir ~s to ~s~n", [Src, Dest]);
event({copy_file, Src, Dest}) ->
    io:format("Copying file ~s to ~s~n", [Src, Dest]).
