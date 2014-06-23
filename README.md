# LambdaPad - Erlang Powered Site Generation, FTW!

LambdaPad is currently a work in process.

LambdaPad can be used to generate a
[static site](http://staticsitegenerators.net). Here's the gist:

- Maintain "data" in various formats
- Maintain "templates" and related static content
- Use an Erlang module to generate a static site

Sound crazy? An Erlang module? Why not YAML, Ruby, JavaScript or some other
super easy to use and understand format?

Because *Erlang* is super easy to use and understand! It's actually much, much
*better* than these other formats and langauges. This projects illustrates
that.

Here's basic "Hello World" web site, as specified in a LambdaPad Erlang module:

```erlang
-module(index).

-include("lpad.hrl").

data() -> #{msg => "Hello World!"}.

site() -> [index].

index(Data) -> page("site/index.html", "index.html", Data).
```

And here's a template:

```html
<html>{{ msg }}</html>
```

To create a site, run `lpad-gen` and you're done!

Sound hard? Follow these super, simple steps to try it:

1. Grab the [lambdapad repo](https://github.com/gar1t/lambdapad) from github

    $ git clone https://github.com/gar1t/lambdapad.git

2. Change to the local `lambdapad` directory and run make

    $ cd lambdapad
	$ make

3. Change to the `samples/hello` directory and run make

    $ cd samples/hello
	$ make

4. Open `lambdapad/samples/hello/site/index.html` in your browser

Take a look at the two files in [samples/hello](samples/hello):

- [samples/hello/index.erl](index.erl)
- [samples/hello/index.html](index.html)

The [Makefile](samples/hello/Makefile) simply runs `lpad-gen` from the local
bin directory. You can make this command available to use anywhere by adding
`LAMBDA_PAD/bin` to you system path, where `LAMBDA_PAD` is the full path to the
local cloned git repo.

## Approach

A site is defined by a single `index.erl` module in the root of a project
directory. The module must provide two functions (these are auto exported):

- `data() -> ...`
- `site() -> ...`

The `data()` function defines the top-level `Data` value that's provided in
calls to generators.

The `site()` function defines a list of generators.

When `lpad-gen` is called, LambdaPad generates a site using the `index.erl`
module:

1. Data is created by calling `data()`
2. Generators are created by calling `site()`
3. Each generator is called with a single `Data` argument, which is the value
   from step 1

That's it.

LambdaPad doesn't like implicit behvior. So there's one, single authoritative
module that is used to create the site.

Data can be loaded from a variety of sources (see Data Loaders below).

Various generators can be used to create site content (see Generators below).

## Data Loaders

### `eterm`

Use the `eterm` function to load a single Erlang term from a file. The term
should be a list or a map.

Here's an example use of the function:

```erlang
data() -> #{params => eterm("params.config")}.
```

Here's the corresponding `params.config` example:

```erlang
#{
  site_title => "My Cool Site",
  site_style => bright_green,
  plugins => [twitter, facebook]
}.
```

Here's how you would use this data in a template:

```html
<html class="{{ params.site_style }}">
<h1>{{ params.site_title }}</h1>
{% for plugin in params.plugins %}
<script src="{{ plugin}}.js"></script>
{% endfor %}
</htm>
```

This is the best way to provide structured data for your site. If you want to
provide markup content, use the `markdown` loader.

Note that you must terminate the single Erlang term with a period `.`.

Below are more examples.

A list of colors:

```erlang
[red, blue, green].
```

A list of shapes, using maps:

```erlang
[
  #{
    shape => circle,
	color => red
  },
  #{
    shape => square,
	color => blue
  },
  #{
    shape => triangle,
	color => green
  }
].
```

Another map of values:

```erlang
#{
   title => "The World According to Garp",
   author => "John Irving",
   published => 1978
}.
```

### `markdown`

This is not completed but will be Real Soon Now.

Here's an example use of the function:

```erlang
data() -> #{about => markdown("about.md")}.
```

Here's the corresponding `about.md` example:

```markdown
---
body_class: bright green
body_id:    about
---
This is a sample markdown file that can be used for the "About" section
of a website. LambdaPad supported markdown lets you:

- Specify text content
- Format text for use in web paged
- Provide parameters/metadata in an optional header section

Pretty cool!
```

Here's how you might use this data:

```html
<html>
<body id="{{ about.body_id }}" class="{{ about.body_class }}">
{{ about.as_html }}
</body>
```

Note: `as_html` is speculative here and just a possible interface.

## Generators

### `page`

The `page` generator writes a rendered template to a file. Call it like this:

```erlang
page(TargetFile, TemplateSource, Data)
```

Paths are relative to the project root.

Sample use:

```erlang
index(Data) -> page("site/index.html", "templates/index.html", Data).
```

### `dirs`, `dir`, `files`, `file`

These generators copy files and directories to a location. Call them like this:

```erlang
dirs(SourcePattern, TargetDir)
dir(Source, TargetDir)
files(SourcePattern, TargetFile)
file(Source, TargetFile)
```

Use these to copy static content from a project directory to a site
location. For example, this might be used to copy all of the static content in
a project:

```erlang
static_content(_Data) -> dirs("static/*", "site").
```

TODO: I'm not happy with this interface. There must be something simpler. I
want to stay away from `cp` because it's imperative feel (though we will
probably have an `ls` so this might be fine?). But this dirs vs files split is
stupid. It should maybe just be `files`. The use of dirs though is that it is a
recursive copy. Anyway, let's see how it goes. Not a hard problem.

## To Do

- Implement data loaders with multiple files (using `ls`)
- Illustrate/doc multi page generation using list comprehension
- Performance optimization (esp. avoid re-generating up-to-date content)
- Story for custom loaders and generators
- Do we want to call "loader" a "data type" - loader sounds stupid
- 
