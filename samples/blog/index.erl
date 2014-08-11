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
      "site/index.html"              => page("index.html", ""),
      "site/posts/index.html"        => page("posts.html", "../"),
      "site/posts/{{post.id}}.html"  => post_pages(Data),
      "site/examples/index.erl.html" => example_page("index.erl"),
      "site/assets/*.css"            => {files, "assets/*.css"}
     }.

page(Template, SiteRoot) ->
    {template, "templates/" ++ Template, #{site_root => SiteRoot}}.

pages(Template, Items, SiteRoot) ->
    {map_template, "templates/" ++ Template, Items, #{site_root => SiteRoot}}.

post_pages(Data) ->
    pages("post.html", {post, plist:value(posts, Data)}, "../").

example_page(ExampleFile) ->
    {template, "templates/example.html",
     #{site_root => "../", example_file => ExampleFile}}.
