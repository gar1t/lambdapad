# To Do Items / Notes

## Tracing

Steal from e2_debug and setup tracing on lpad modules if LPAD_TRACE_FILE env
var is set and non-0. This is better than peppering the code with logging.

## Overwriting source files

The way we're requiring a "site" directory to avoid obliterating source files
is very dangerous. This is a bit tough, as we don't want implicit
behavior. E.g. we don't want to hard code a "site" directory that we generate
magically.

We also have this problem with the hard coding of "index.erl" as the index
file.

Hard-coded, implicit behavior is very un-Lambdapad.

Options:

- Look in .lpad for a config file that specifies the default index, the target
  directory, and any other options that might evolve here

- Two options to lpad-gen: INDEX_FILE and SITE_DIR

- Continue to hard-code index as the module and make it part of the site

### Define in .lpad/config

```erlang
#{
  index_module => index,
  site_root    => "site"
}.
```

I don't like this as it's starting to feel like these other generators that
have important config all over the place, hiding important information.

It also leaves the site definition looking like this:

```erlang
site(_) ->
    [{"index.html", {template, "index.html"}}].
```

Where does 'index.html' go? Who knows? In frustration you smash your
keyboard. It's not a good option.

### Args to lpad-gen

Same problem as with `.lpad/config`.

### Define in site/1

```erlang
site(_) ->
    {"site",
     [{"index.html", {template, "index.html"}}]}.
```

### Not a problem

The fact is that any top-level directory can point to source and end up
obliterating it. Let's keep it simple. This is pretty clear:

```erlang
site(_) ->
     [{"site/index.html", {template, "index.html"}}].
```

We could issue a warning if content (target) is written to the root directory,
as this is a bad idea.

I'm inclined to this option.

## Custom Filters

It'd be easy to include index as a template filter module, making it possible
to do this:

```erlang
-module(index).

my_filter(Val) -> "I filter you: " ++ Val.
```

Use in a template:

```
{{ msg|my_filter }}
```

``index`` could also be extended to support:

```erlang
template_filters(_) -> [my_custom_filters].

## List generator

We want to invoke another generator for each item in a list. Something like
this:

```erlang
data(_) -> #{colors => [red, blue, green]}.

site(_) -> #{ "site/?.html" => {list, colors, {template, "color.html"}} }.
```

Lots to be sorted out here.

## Custom Loaders and Generators

We could introduce a phase in the site generation for custom loaders and
generators as well as filters (above). Each could be a separate function or
perhaps a single function with a type arg.

Or the index itself could define handle_data_spec and handle_generator_spec.

## Consistent Samples

Some samples use property lists for site, some use maps.

## List base templates as sources

If index.html extends base.html, then any targets that depend on index.html
also depend on base.html.

## Create an index for samples

On make samples, use an index.erl to create an index.html that has links to
each of the samples.

## Support trace patterns for functions

Look for MOD[:FUN[/ARITY]] pattern in LPAD_TRACE env and setup tracing
accordingly.

## Data loader filters and maps

We need to support filters and maps at the data loader spec level. There's
currently no way to do this, short of a template filter, or hacking the "vars"
going into a generator, or filtering the list going into a template map.

E.g

    {markdown, "speakers/*.md", fun confirmed_speaker/1}

## Investigate index filter getting called twice when used from template

To recreate, create a filter that prints to stdout in index.erl and include it
once in a template. On lpad-gen it outputs twice. Why?
