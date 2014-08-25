-module(index).

%data(_) -> {json, "hello.json"}.
data(_) -> "hello.json".

site(_) -> [{"site/index.html", {template, "index.html"}}].
