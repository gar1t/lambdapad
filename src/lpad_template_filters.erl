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

-export([read_file/1,
         read_file/2,
         markdown_to_html/1,
         sort/1, sortasc/1, sortdesc/1,
         sort/2, sortasc/2, sortdesc/2,
         find/2]).

-define(toi(L), list_to_integer(L)).

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
%%% markdown_to_html
%%%-------------------------------------------------------------------

markdown_to_html(undefined) -> "";
markdown_to_html([{_, _}|_]=MD) ->
    File = proplists:get_value('__file__', MD),
    lpad_markdown:to_html(File);
markdown_to_html(File) ->
    lpad_markdown:to_html(File).

%%%-------------------------------------------------------------------
%%% sort, sortasc, sortdesc
%%%-------------------------------------------------------------------

sort(List) -> sortasc(List).

sort(List, Attr) -> sortasc(List, Attr).

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
    fun(P1, P2) -> plist:value(Attr, P1) < plist:value(Attr, P2) end;
sort_fun(desc, Attr) ->
    fun(P1, P2) -> plist:value(Attr, P1) > plist:value(Attr, P2) end.

to_list(L) when is_list(L)   -> L;
to_list(B) when is_binary(B) -> binary_to_list(B). 

%%%-------------------------------------------------------------------
%%% find
%%%-------------------------------------------------------------------

find(undefined, _Name) -> undefined;
find(_List, undefined) -> undefined;
find(List, Name) ->
    find_impl(List, to_list(Name)).

find_impl([Item|Rest], Name) ->
    maybe_find(item_matches(Item, Name), Item, Rest, Name);
find_impl([], _Name) -> undefined.

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

maybe_find(true, Item, _Rest, _Name) -> Item;
maybe_find(false, _Item, Rest, Name) -> find_impl(Rest, Name).
