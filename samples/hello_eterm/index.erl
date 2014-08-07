-module(index).

data(_) -> {eterm, "hello.config"}.

site(_) -> [{"site/index.html", {string, "<html>{{ msg }}</html>"}}].
