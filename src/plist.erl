-module(plist).

-export([value/2, value/3]).

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
