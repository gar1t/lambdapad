-module(index).

data(_) ->
    #{ msg              => "Hello World!",
       from_eterm       => {eterm,    "hello.config"},
       from_json        => {json,     "hello.json"},
       from_markdown    => {markdown, "hello.md"}
     }.

site(_) ->
    #{"site/index.html" => {template, "index.html"},
      "site/*.css"      => {files,    "*.css"}
     }.
