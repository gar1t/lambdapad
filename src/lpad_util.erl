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

-export([printable_str/1]).

printable_str(Str) ->
    [printable_char(Ch) || Ch <- Str].

printable_char(Ch) when Ch >= 8, Ch =< 255 -> Ch;
printable_char(_) -> 168.
