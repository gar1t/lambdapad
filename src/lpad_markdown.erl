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

-module(lpad_markdown).

-export([load/1]).

load(File) ->
    handle_markdown_file(file:read_file(File), File).

handle_markdown_file({ok, Bin}, _File) ->
    parse(Bin);
handle_markdown_file({error, Err}, File) ->
    error({read_file, File, Err}).

parse(Bin) ->
    handle_split_markdown(split_markdown(Bin), Bin).
    
split_markdown(Bin) ->
    Pattern = "^---\\h*\\v+(.*)\\h*\\v*---(?:\\h*\\v+(.*))?",
    Opts = [{capture, all_but_first, binary}, dotall],
    handle_split_headers_match(re:run(Bin, Pattern, Opts), Bin).

handle_split_headers_match({match, [HeadersBin, BodyBin]}, _FileBin) ->
    {HeadersBin, BodyBin};
handle_split_headers_match({match, [HeadersBin]}, _FileBin) ->
    {HeadersBin, undefined};
handle_split_headers_match(nomatch, FileBin) ->
    {undefined, FileBin}.

handle_split_markdown({Headers, Body}, Raw) ->
    {parse_headers(Headers), parse_markdown(Body), Raw}.

parse_headers(Bin) ->
    acc_headers(header_lines(Bin), #{}).

header_lines(Bin) -> re:split(Bin, "\\v+").

acc_headers([Line|Rest], Headers) ->
    acc_headers(Rest, maybe_acc_header(Line, Headers));
acc_headers([], Headers) -> Headers.

maybe_acc_header(Line, Headers) ->
    handle_header_split(split_header(Line), Headers).

split_header(Line) -> re:split(Line, "\\h*:\\h*").

handle_header_split([Name, Val], Headers) ->
    NameAsStr = binary_to_list(Name),
    ValAsStr = binary_to_list(Val),
    maps:put(NameAsStr, ValAsStr, Headers);
handle_header_split(_, Headers) ->
    Headers.

parse_markdown(Bin) ->
    markdown:conv(binary_to_list(Bin)).
