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
      "site/posts/{{id}}.html"        => post_pages(Data),
      "site/examples/index.erl.html"  => example_page("index.erl"),
      "site/assets/*.css"             => {files, "assets/*.css"}
     }.

page(Template, SiteRoot) ->
    {template, "templates/" ++ Template, #{site_root => SiteRoot}}.

pages(Items, Template, SiteRoot) ->
    {map_template, Items, "templates/" ++ Template, #{site_root => SiteRoot}}.

post_pages(Data) ->
    pages(plist:value(posts, Data), "post.html", "../").

example_page(ExampleFile) ->
    {template, "templates/example.html",
     #{site_root => "../", example_file => ExampleFile}}.
