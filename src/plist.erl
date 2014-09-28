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

-export([value/2, value/3,
         has_value/2,
         traverse/2, traverse/3,
         find/3, find/4,
         filter_by_value/3,
         convert_maps/1]).

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

has_value(Name, List) ->
    lists:keymember(Name, 1, List).

traverse([Name|Rest], List) ->
    traverse(Rest, value(Name, List));
traverse([], Value) ->
    Value.

traverse([Name|Rest], List, Default) ->
    maybe_traverse(value(Name, List, '$undefined'), Rest, Default);
traverse([], Value, _Default) ->
    Value.

maybe_traverse('$undefined', _Rest, Default) ->
    Default;
maybe_traverse(List, Rest, Default) ->
    traverse(Rest, List, Default).

find(Key, Value, [Item|Rest]) ->
    maybe_item(
      value(Key, Item, '$undefined'),
      Value, Item, Key, Rest);
find(Key, Value, []) ->
    error({no_such_item, {Key, Value}}).

find(Key, Value, [Item|Rest], Default) ->
    maybe_item(
      value(Key, Item, '$undefined'),
      Value, Item, Key, Rest, Default);
find(_Key, _Value, _List, Default) ->
    Default.

maybe_item(Value, Value,  Item, _Key, _Rest) -> Item;
maybe_item(    _, Value, _Item,  Key,  Rest) ->
    find(Key, Value, Rest).

maybe_item(Value, Value,  Item, _Key, _Rest, _Default) -> Item;
maybe_item(    _, Value, _Item,  Key,  Rest,  Default) ->
    find(Key, Value, Rest, Default).

filter_by_value(Name, List, Value) ->
    EqualsValue = fun(Item) -> value(Name, Item, '$undefined') == Value end,
    lists:filter(EqualsValue, List).

convert_maps(Map) when is_map(Map) ->
    [convert_maps(Item) || Item <- maps:to_list(Map)];
convert_maps(List) when is_list(List) ->
    [convert_maps(Item) || Item <- List];
convert_maps({Key, Value}) ->
    {convert_maps(Key), convert_maps(Value)};
convert_maps(Other) ->
    Other.
