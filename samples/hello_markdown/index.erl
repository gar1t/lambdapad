-module(index).

-include("lpad.hrl").

data() -> markdown("hello.md").

site(_) -> [index].

index(Data) -> page("site/index.html", "index.html", Data).
