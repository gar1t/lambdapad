-module(index).

-include("lpad.hrl").

data() -> #{msg => "Hello World!"}.

site(_Data) -> [index].

index(Data) -> page("site/index.html", "index.html", Data).
