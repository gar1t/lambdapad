-module(lpad_future).

-export([altsite/1]).

altsite(Map) ->
    maps:map(fun altsite/2, Map).

altsite(_, #{template := Template, map_list := List}=Spec) ->
    {template_map, Template, {map_item(Spec), List}, Spec};
altsite(_, #{template := Template}=Spec) ->
    {template, Template, Spec};
altsite(_, #{dir := Dir}) ->
    {dir, Dir}.

map_item(#{map_item := Item}) -> Item;
map_item(_) -> item.
