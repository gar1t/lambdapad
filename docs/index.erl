-module(index).

%===================================================================
% Site data
%===================================================================

data(_) ->
    #{project  => {eterm,    "project.config"},
      articles => {markdown, "articles/*.md"},
      navbar   => {eterm,    "navbar.config"}
     }.

%===================================================================
% Site structure
%===================================================================

site(_) ->
    #{"dist/index.html" =>
          {template, "templates/index.html"},

      "dist/getting-started/index.html" =>
          {template, "templates/getting-started.html",
           #{root => "../",
             active_menu => "getting-started"}},

      "dist/references/index.html" =>
          {template, "templates/references.html",
           #{root => "../",
             active_menu => "references"}},

      "dist/css"     => {dir, "static/css"},
      "dist/js"      => {dir, "static/js"},
      "dist/fonts"   => {dir, "static/fonts"},
      "dist/images"  => {dir, "static/images"}
     }.

%===================================================================
% docs_nav filter
%===================================================================

docs_nav(HTML) ->
    ["<ul class=\"nav\">", docs_nav_items(headings(HTML)), "</ul>"].

headings(HTML) ->
    Pattern = "<h1 id=\"(.+?)\">(.*?)<",
    Opts = [{capture, all_but_first, list}, global],
    handle_headings_match(re:run(HTML, Pattern, Opts)).

handle_headings_match({match, Matches}) ->
    [{Id, Title} || [Id, Title] <- Matches];
handle_headings_match(nomatch) ->
    [].

docs_nav_items([First|Rest]) ->
    [doc_nav_active_li(First), [doc_nav_li(I) || I <- Rest]];
docs_nav_items([]) -> [].

doc_nav_active_li(Item) ->
    ["<li class=\"active\">", doc_nav_link(Item), "</li>"].

doc_nav_li(Item) ->
    ["<li>", doc_nav_link(Item), "</li>"].

doc_nav_link({Id, Title}) ->
    ["<a href=\"#", Id, "\">", Title, "</a>"].
