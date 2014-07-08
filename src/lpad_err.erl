-module(lpad_err).

-export([format/1]).

format(Err) ->
    io:format(standard_error, "~p~n", [Err]).
