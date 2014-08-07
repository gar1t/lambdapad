-module(index).

data(_) -> {json, "hello.json"}.

site(_) -> [{"site/index.html", {template, "index.html"}}].
