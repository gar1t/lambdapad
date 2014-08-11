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

-module(plist).

-export([value/2, value/3, filter_by_value/3]).

value(Name, List) ->
    case lists:keyfind(Name, 1, List) of
        {_, Value} -> Value;
        false -> error({required_value, Name})
    end.

value(Name, List, Default) ->
    case lists:keyfind(Name, 1, List) of
        {_, Value} -> Value;
        false -> Default
    end.

filter_by_value(Name, List, Value) ->
    EqualsValue = fun(Item) -> value(Name, Item, '$undefined') == Value end,
    lists:filter(EqualsValue, List).
