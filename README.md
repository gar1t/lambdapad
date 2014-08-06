# LambdaPad: Erlang Powered Site Generation, FTW!

LambdaPad is currently a work in process.

LambdaPad can be used to generate a
[static site](http://staticsitegenerators.net). Here's the gist:

- Maintain *data* in various formats
- Maintain *templates* and related *static content*
- Use an Erlang module to generate a static site

Sound crazy? An Erlang module? Why not YAML, Ruby, JavaScript or some other
super easy to use and understand format?

Because *Erlang* is super easy to use and understand! It's actually much, much
*better* than these other formats and languages. This project hopes to
illustrate that.

Here's basic "Hello World" web site, as specified in a LambdaPad Erlang module:

```erlang
-module(index).

-include("lpad.hrl").

data() -> #{msg => "Hello World!"}.

site(_Data) -> [index].

index(Data) -> page("site/index.html", "index.html", Data).
```

And here's a template:

```html
<html>{{ msg }}</html>
```

To create a site, run `lpad-gen` in your project directory and you're done!

Sound hard? Follow these super, simple steps to try it:

1. Grab the [lambdapad repo](https://github.com/gar1t/lambdapad) from github:

	```bash
	$ git clone https://github.com/gar1t/lambdapad.git
	```

2. Change to the local `lambdapad` directory and run make:

	```bash
	$ cd lambdapad
	$ make
	```

3. Change to the `samples/hello` directory and run make:

	```
	$ cd samples/hello
	$ make
	```

4. Open `lambdapad/samples/hello/site/index.html` in your browser

To understand what's going on, take a look at the files in
[samples/hello](samples/hello):

- [index.erl](samples/hello/index.erl)
- [index.html](samples/hello/index.html)
- [Makefile](samples/hello/Makefile)

`index.erl` is the same as the example above. Let's look at each line in turn.

```erlang
-module(index).
```

This is required for Erlang modules. It's not that bad.

```erlang
-include("lpad.hrl").
```

This imports a number of functions that will make your life much, much
easier. It's not that bad.

```erlang
data() -> #{msg => "Hello World!"}.
```

This is an Erlang function named `data` and it takes no arguments. It's
required. It provides data that is used by site generators and, in particular,
templates. This can get pretty fancy with multiple data sources coming from
Erlang terms, markdown, and json.

```erlang
site(_Data) -> [index].
```

This is another Erlang function. It's required and must be named `site` and
take a single argument, which is the value returned from `data()`. It returns a
list of site generators. A generator may refer to a function defined in
`index.erl` or an Erlang tuple of a module and function (e.g. `{mymod,
index}`).

The argument `_Data` has a leading underscore to let Erlang know it's not used
anywhere in the function. It's possible to use the data to parameterize the
site structure.

```erlang
index(Data) -> page("site/index.html", "index.html", Data).
```

This is a generator that takes a single `Data` argument, which is the value
returned from `data()`. This particular implementation calls the `page`
function to create a file `site/index.html` using a template `index.html`. The
template has access to the fields in Data. In this case there's a single `msg`
field with value `"Hello World!"`.

Here's the `index.html` template:

```html
<html>{{ msg }}</html>
```

LambdaPad templates support the
[Django templating language](https://docs.djangoproject.com/en/dev/ref/templates/builtins/).

`Makefile` simply runs `lpad-gen` from the local bin directory. You can make
this command available to use anywhere by adding `LAMBDA_PAD/bin` to you system
path, where `LAMBDA_PAD` is the full path to the local cloned git repo.

## Approach

A site is defined by a single `index.erl` module in the root of a project
directory. The module must provide two functions (which are auto exported):

- `data() -> ...`
- `site() -> ...`

The `data()` function defines the top-level *Data* value that's provided in
calls to generators.

The `site()` function defines a list of generators.

Site generators are typically implemented in the site index module. They take a
single argument, which is the value that was returned by `data()`.

When `lpad-gen` is called, LambdaPad generates a site using the `index.erl`
module:

1. Data is created by calling `data()`
2. Generators are created by calling `site()`
3. Each generator is called with a single `Data` argument, which is the value
   from step 1

That's it.

LambdaPad doesn't like implicit behavior. So there's one, single authoritative
module that is used to create the site. There's nothing weird lurking in
subdirectories, metadata.yaml files, or other disturbing places.

Data can be loaded from a variety of sources (see
[Data Loaders](#data-loaders)) below).

Various generators can be used to create site content (see
[Generators](#generators) below).

## Data Loaders

### `eterm(File)`

Use the `eterm` function to load a single Erlang term from a file. The term
should be a list or a map.

Here's how the function might be used:

```erlang
data() -> #{params => eterm("params.config")}.
```

Here's an example of what `params.config` might look like:

```erlang
#{
  site_title => "My Cool Site",
  site_style => bright_green,
  plugins => [twitter, facebook]
}.
```

And a template that uses this data:

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

See the [page generator](#pagetarget-template-data) for details on generating a
page using a template and data.

Note that you must terminate the single Erlang term with a period `.`

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

See the [samples/hello](samples/hello) for a working example.

### `markdown(File)`

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
  {{ about.HTML }}
  </body>
</html>
```

See the [samples/hello_markdown](samples/hello_markdown) for a working example.

### `json(File)`

Here's an example use of the function:

```erlang
data() -> json("data.json").
```

Here's the corresponding `data.json` example:

```json
{
  "msg": "This data came from the venerable JSON format!"
  "colors": ["red", "green", "blue"]
}
```

Here's how you might use this data:

```html
<html>
  <p>{{ msg }}</p>
  <p>See the pretty colors:</p>
  <ul>
    {% for color in colors %}
	<li>{{ color }}
	{% endfor %}
  </ul>
  </body>
</html>
```

See the [samples/hello_json](samples/hello_json) for a working example.

## Generators

### `page(Target, Template, Data)`

The `page` generator writes a rendered template to a file. Paths are relative
to the project root.

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

# System Requirements

- Non-windows (at the moment)
- Developer configure/make "big boy" environment
- Erlang R17 (need map support, the way it is)

## To Do

- Implement data loaders with multiple files (using `ls`)
- Illustrate/doc multi page generation using list comprehension
- Performance optimization (esp. avoid re-generating up-to-date content)
- Story for custom loaders and generators
- Do we want to call "loader" a "data type" - loader sounds stupid
- The term "generator" is overloaded - I'm using it both for site level
  functions and the functions that actually create/copy files. This is
  confusing.
