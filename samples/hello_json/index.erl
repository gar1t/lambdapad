-module(index).

-include("lpad.hrl").

data() -> json("hello.json").

site(_) -> [index].

index(Data) -> page("site/index.html", "index.html", Data).
