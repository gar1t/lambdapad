-module(index).

-include("lpad.hrl").

data() -> markdown("hello.md").

site(_) -> [index, css].

index(Data) -> page("site/index.html", "index.html", Data).

css(_) -> files("styles.css", "site/").
