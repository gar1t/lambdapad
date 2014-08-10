-module(index).

%-------------------------------------------------------------------
% Data
%-------------------------------------------------------------------

data(_) ->
    #{
      blog          => {eterm,    "blog.config"},
      snippets      => {markdown, "snippets/*.md"},
      posts         => {markdown, "posts/*.md"}
     }.

%-------------------------------------------------------------------
% Site structure
%-------------------------------------------------------------------

site(Data) ->
    #{
      "site/index.html"               => page("index.html", ""),
      "site/posts/index.html"         => page("posts.html", "../"),
%%      "site/posts/{{__name__}}.html"  => post_pages(Data),
      "site/assets/*.css"             => {files, "assets/*.css"}
     }.

page(Template, SiteRoot) ->
    {template, "templates/" ++ Template, #{site_root => SiteRoot}}.

%% pages(Items, Template, SiteRoot) ->
%%     {map_template, Items, "templates/" ++ Template, #{site_root => SiteRoot}}.

%% post_pages(Data) ->
%%     pages(proplists:get_value(posts, Data), "post.html", "../").

%-------------------------------------------------------------------
% Filter: recent_posts
%-------------------------------------------------------------------

recent_posts(Posts) ->
    first(5, posts_by_date(Posts)).

posts_by_date(Posts) ->
    lists:sort(fun post_date_cmp/2, Posts).

post_date_cmp({_, P1}, {_, P2}) ->
    proplists:get_value("date", P1) > proplists:get_value("date", P2).

first(N, Items) -> lists:sublist(Items, N).
