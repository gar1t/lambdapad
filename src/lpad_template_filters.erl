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

-module(lpad_template_filters).

-export([read_file/1, read_file/2,
         filename/1, basename/1,
         markdown_to_html/1, strip_p/1,
         render/1,
         sort/1, sortasc/1, sortdesc/1,
         sort/2, sortasc/2, sortdesc/2,
         nsort/1, nsortasc/1, nsortdesc/1,
         nsort/2, nsortasc/2, nsortdesc/2,
         filter/2, get/2]).

-define(toi(L), list_to_integer(L)).

%%%-------------------------------------------------------------------
%%% Shared / utils
%%%-------------------------------------------------------------------

to_list(L) when is_list(L)   -> L;
to_list(B) when is_binary(B) -> binary_to_list(B).

%%%-------------------------------------------------------------------
%%% read_file
%%%-------------------------------------------------------------------

read_file(undefined) -> "";
read_file(File) ->
    handle_file_read(file:read_file(File), File).

read_file(undefined, _) -> "";
read_file(File, LinesSpec) ->
    {First, Last} = parse_lines(LinesSpec),
    F = open_file(File),
    Lines = slice(read_lines(F), First, Last),
    close_file(F),
    Lines.

parse_lines(Spec) ->
    Pattern = "^(\\d+)?:(-?\\d+)?$",
    Opts = [{capture, all_but_first, list}],
    match_to_lines(re:run(Spec, Pattern, Opts), Spec).

open_file(File) ->
    handle_file_open(file:open(File, [read]), File).

handle_file_open({ok, F}, _Src) -> F;
handle_file_open({error, Err}, Src) ->
    error({file_read, Src, Err}).

match_to_lines({match, []},            _Spec) -> {1,           infinity};
match_to_lines({match, [First]},       _Spec) -> {?toi(First), infinity};
match_to_lines({match, ["",    Last]}, _Spec) -> {1,           ?toi(Last)};
match_to_lines({match, [First, Last]}, _Spec) -> {?toi(First), ?toi(Last)};
match_to_lines(nomatch, Spec)                 ->  error({lines_spec, Spec}).

handle_file_read({ok, Bin}, _File) ->
    Bin;
handle_file_read({error, Err}, File) ->
    error({file_read, File, Err}).

read_lines(F) ->
    acc_lines(next_line(F), []).

next_line(F) ->
    {io:get_line(F, ""), F}.

acc_lines({eof, _}, Lines) -> lists:reverse(Lines);
acc_lines({Line, F}, Lines) -> acc_lines(next_line(F), [Line|Lines]).

slice(List, First, Last) when First < 1 ->
    slice(List, 1, Last);
slice(List, First, infinity) ->
    lists:sublist(List, First, length(List) - First);
slice(List, First, Offset) when Offset < 0 ->
    slice(List, First, max(0, length(List) + Offset));
slice(_List, First, Last) when First > Last ->
    [];
slice(List, First, Last) ->
    lists:sublist(List, First, Last - First + 1).

close_file(F) ->
    ok = file:close(F).

%%%-------------------------------------------------------------------
%%% filename, basename
%%%-------------------------------------------------------------------

filename(L) when is_list(L) ->
    plist:value('__file__', L, undefined);
filename(_) ->
    undefined.

basename(Val) ->
    maybe_basename(filename(Val)).

maybe_basename(undefined) -> undefined;
maybe_basename(Path) ->
    filename:basename(Path, filename:extension(Path)).

%%%-------------------------------------------------------------------
%%% render
%%%-------------------------------------------------------------------

render(undefined) -> "";
render(Context) -> lpad_template:render_string(Context, []).

%%%-------------------------------------------------------------------
%%% markdown_to_html
%%%-------------------------------------------------------------------

markdown_to_html(undefined) -> "";
markdown_to_html(Context) -> lpad_markdown:to_html(Context).

%%%-------------------------------------------------------------------
%%% strip_p
%%%-------------------------------------------------------------------

strip_p(S) ->
    Opts = [{capture, all_but_first, binary}, dotall],
    case re:run(S, "<p>(.*)</p>", Opts) of
        {match, Stripped} -> Stripped;
        nomatch -> S
    end.

%%%-------------------------------------------------------------------
%%% sort, sortasc, sortdesc
%%%-------------------------------------------------------------------

sort(List) ->
    sortasc(List).

sort(List, Attr) ->
    sortasc(List, Attr).

sortasc(List) ->
    lists:sort(sort_fun(asc), List).

sortasc(List, Attr) ->
    lists:sort(sort_fun(asc, to_list(Attr)), List).

sortdesc(List) ->
    lists:sort(sort_fun(desc), List).

sortdesc(List, Attr) ->
    lists:sort(sort_fun(desc, to_list(Attr)), List).

sort_fun(asc) ->
    fun(I1, I2) -> I1 < I2 end;
sort_fun(desc) ->
    fun(I1, I2) -> I1 > I2 end.

sort_fun(asc, Attr) ->
    fun(P1, P2) -> sort_val(Attr, P1) < sort_val(Attr, P2) end;
sort_fun(desc, Attr) ->
    fun(P1, P2) -> sort_val(Attr, P1) > sort_val(Attr, P2) end.

sort_val(Attr, Proplist) ->
    Default = plist:value(existing_atom(Attr), Proplist, undefined),
    plist:value(Attr, Proplist, Default).

%%%-------------------------------------------------------------------
%%% nsort, nsortasc, nsortdesc
%%%-------------------------------------------------------------------

nsort(List) ->
    nsortasc(List).

nsort(List, Attr) ->
    nsortasc(List, Attr).

nsortasc(List) ->
    lists:sort(nsort_fun(asc), List).

nsortasc(List, Attr) ->
    lists:sort(nsort_fun(asc, to_list(Attr)), List).

nsortdesc(List) ->
    lists:sort(nsort_fun(desc), List).

nsortdesc(List, Attr) ->
    lists:sort(nsort_fun(desc, to_list(Attr)), List).

nsort_fun(asc) ->
    fun(I1, I2) -> try_number(I1) < try_number(I2) end;
nsort_fun(desc) ->
    fun(I1, I2) -> try_number(I1) > try_number(I2) end.

nsort_fun(asc, Attr) ->
    fun(P1, P2) -> nsort_val(Attr, P1) < nsort_val(Attr, P2) end;
nsort_fun(desc, Attr) ->
    fun(P1, P2) -> nsort_val(Attr, P1) > nsort_val(Attr, P2) end.

nsort_val(Attr, Proplist) ->
    Default = plist:value(existing_atom(Attr), Proplist, undefined),
    try_number(plist:value(Attr, Proplist, Default)).

try_number(N) when is_number(N) -> N;
try_number(L) when is_list(L)   -> try_list_to_number(L);
try_number(B) when is_binary(B) -> try_list_to_number(binary_to_list(B));
try_number(Other)               -> Other.

try_list_to_number(L) ->
    try_convert([fun list_to_integer/1, fun list_to_float/1], L).

try_convert([Fun|Rest], Val) ->
    try
        Fun(Val)
    catch
        _:_ ->try_convert(Rest, Val)
    end;
try_convert([], Val) -> Val.

%%%-------------------------------------------------------------------
%%% filter
%%%-------------------------------------------------------------------

filter(undefined, _Query) -> undefined;
filter(List, Query) ->
    lists:filter(filter_item_fun(split_query(Query)), List).

split_query(Query) ->
    Pattern = "(.+?)\s*(=)\s*(.*)",
    case re:run(Query, Pattern, [{capture, all_but_first, list}]) of
        {match, [Name, Op, Value]} -> {Name, Op, Value};
        nomatch -> false
    end.

filter_item_fun(Query) ->
    fun(Item) -> match_item(Item, Query) end.

match_item(Item, {Attr, Op, Value}) ->
    apply_filter_op(Op, try_item_val(Item, Attr), Value);
match_item(_Item, false) -> false.

try_item_val(_Item, '$undefined') -> '$undefined';
try_item_val(Item, Name) when is_atom(Name) ->
    plist:value(Name, Item, '$undefined');
try_item_val(Item, Name) ->
    case plist:value(Name, Item, '$undefined') of
        '$undefined' -> try_item_val(Item, existing_atom(Name));
        Value -> Value
    end.

existing_atom(Name) ->
    try
        erlang:list_to_existing_atom(Name)
    catch
        _:badarg -> '$undefined'
    end.

apply_filter_op("=", Value, Value) -> true;
apply_filter_op(Op, Value, Target) when is_atom(Value) ->
    apply_filter_op(Op, atom_to_list(Value), Target);
apply_filter_op(_Op, _Value, _Target) -> false.

%%%-------------------------------------------------------------------
%%% get
%%%-------------------------------------------------------------------

get(undefined, _Name) -> undefined;
get(List, Name) ->
    get_impl(List, to_list(Name)).

get_impl([Item|Rest], Name) ->
    maybe_get(item_matches(Item, Name), Item, Rest, Name);
get_impl([], _Name) -> undefined.

item_matches(Item, Name) when is_list(Item) ->
    item_matches_file(plist:value('__file__', Item, undefined), Name).

item_matches_file(undefined, _Name) ->
    false;
item_matches_file(File, Name) ->
    try_match(file_name_options(File), Name).

file_name_options(File) ->
    BaseName = filename:basename(File),
    NameWithoutExt = filename:basename(BaseName, filename:extension(BaseName)),
    [BaseName, NameWithoutExt].

try_match([Name|_], Name) -> true;
try_match([_|Rest], Name) -> try_match(Rest, Name);
try_match([],      _Name) -> false.

maybe_get(true, Item, _Rest, _Name) -> Item;
maybe_get(false, _Item, Rest, Name) -> get_impl(Rest, Name).
