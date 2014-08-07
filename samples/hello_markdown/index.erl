-module(index).

data(_) -> {markdown, "hello.md"}.

site(_) ->
    [{"site/index.html", {template, "index.html"}},
     {"site/styles.css", {file, "styles.css"}}].
