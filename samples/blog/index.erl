-module(index).

data(_) ->
    #{
      blog     => {eterm,    "blog.config"},
      snippets => {markdown, "snippets/*.md"},
      posts    => {markdown, "posts/*.md"}
     }.

site(Data) ->
    #{
      "site/index.html" =>
          {template, "templates/index.html",
           #{site_root => ""}},

      "site/posts/index.html" =>
          {template, "templates/posts.html",
           #{site_root => "../"}},

      "site/posts/{{post.id}}.html" =>
          {template_map, "templates/post.html", {post, posts(Data)},
           #{site_root => "../"}},

      "site/examples/index.erl.html" =>
          {template, "templates/example.html",
           #{site_root => "../",
             example_file => "index.erl"}},

      "site/assets/*.css" =>
          {files, "assets/*.css"}
     }.

posts(Data) -> plist:value(posts, Data).
