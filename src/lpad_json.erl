-module(lpad_json).

-export([load/1]).

load(File) ->
    handle_json_file(file:read_file(File), File).

handle_json_file({ok, Bin}, _File) ->
    struct_to_maps(jiffy:decode(Bin));
handle_json_file({error, Err}, File) ->
    error({read_file, File, Err}).

struct_to_maps({List}) ->
    maps:from_list([{Key, struct_to_maps(Val)} || {Key, Val} <- List]);
struct_to_maps(Other) ->
    Other.
