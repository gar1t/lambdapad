# Notes on rethinking LambdaPad

I don't like how much work you need to do in the index.erl - it's a
pain. Yes it's nice to be explicit, but it's too much work.

Having used Jekyll for GitHub pages I can say it's truly a horrific
tool and barely gets the job done, and that with unending frustration.

LambdaPad v1 is a nice start and demonstrates that Erlang is fine for
building this sort of tool.

So now what?

Here's what I'm thinking:

## Arbitrary directory structure

Let the user store content any damn way she pleases - not requiring
idiotic underscores because of presumed default behavior.

If I want to store "posts" I can do this:

```
$ mkdir posts
```

and then create posts as Markdown files.

## Content referencing and iteration

We need some primitives to iterate and access content.

Here's what we can discover on disk:

- Markdown file (*.md): imply a map of key/name values as metadata, file
  attributes, and renderably content

- JSON file (*.json): whatever's in the JSON file

- Erlang config file (*.config): whatever's in the config file

- Directory: imply lists of whatever's in the directory

These artifacts are collectively referred to as *content*. Each
content item supports a map interface. Files always provide file
related attributes. How are these differentiated from content
attributes? E.g. if as are referencing

## Drop proplists

Let's go full on with maps and forget support of proplists, unless the
proplists are defined as Erlang content.

## Keep directives super simple and what people want

Maybe this for static content":

```
site() -> [{static, "assets", "site/assets"}].
```

### Comment May 2, 2017

This still feels wonky. What if a site definition simply mapped
content to some *thing*. E.g. the "assets" directory represents
"static content". We can "tag" is thusly:

```
site() ->
    #{dir => "site",
      content =>
        [{"assets", [dir]}]
     }.
```

I'm not sure how `static` is different from simply copying a directory as-is.

We'd need to support registerable handlers. Something would know about
`static` and do the right thing.

Tags could have optional configuration. E.g.

```
{dir, #{target => "assets2", exclude => "*~"}}
```

## Site definition must be a list of directives

This needs to be a list that's evaluated in sequence, not a map!

## More easily customizable

Make Erlang modules accessible:

```
libs() -> ["libs"].
```

Each dir in the list may contain a series of Erlang source files
(*.erl) or precompiled binaries (*.beam) that are loaded and used
based on the functions they export.

## Get dependencies right

Build this as a tree of dependencies from the start.

## Data is loaded from the file system

I should be able to do this:

```
{% for post in posts %}
{{post.title}}
{% endfor %}
```

without having to define `posts` in `index.erl`.

## Preprocessor

This is a big maybe, but maybe we should support a preprocessor stage
that can create content that then be used in content rendering.

```
preprocess() ->
    [{posts, format, ["posts", "posts-formatted"]}].
```

This would call a function `format` in a `posts` module (located in a
lib dir) and provide the two arguments. The idea is that this function
would apply formatting to the docs in "posts" and store corresponding
formatted content in "posts-formatted".

The "posts-formatted" would be a non-source dir (e.g. included in
`.gitignore`).

This feature would only make sense if it managed dependencies.

So it might need to be in the form of a generator:

```
preprocess() ->
    [{{posts, format}, "posts/*.md", "posts-formatted/*.config"}].
```

## Is this just a Makefile?

Maybe this is just an Erlang term that is used to generate a Makefile!

We'd obviously provide utilities for rendering content.

Custom functions could be written in Erlang - for formatting, etc.

## Translate hyphens ("-") to underscores ("_") as needed

I want to store my files in directories and file names containing
hyphens, not underscores. Our data access facility should know this
and just work.

## Examples

### Static content

```
site() -> [{static, "assets", "site/assets"}].
```

### Whole site from single dir

```
site() ->
    [{pages, "pages", "site"},
     {static, "static", site/static"}].
```

#### Comment May 2, 2017

```
site() ->
    #{content =>
      [{"pages", [pages]},
       {"static", [dir]}]
     }.
```

### Top level functions in index.erl

```
site() -> [...].

libs() -> [...].
```
