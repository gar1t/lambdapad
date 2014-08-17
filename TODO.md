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

## Smarter data dependencies for templates

This is a hard problem. Atm, templates specify the generic `'$data'`
dependency, which makes them dependent on *all* data sources, not just the ones
they actually use.

But how do we know what sources a template actually uses? These sources come by
way of a single context value, which is made of up all the data sources. We'd
need to be very smart about tracking values that are used by the template and
deference their sources.

As it stands now, a change to a single *.md file will cause a regeneration of
all template content. This is terrible behavior. I'd expect just the effected
targets to be regenerated.

This is a tough one. I *think* we need to hack the template generator scheme
collect (or broadcast) the `__file__` values of objects are they're "read"
(e.g. wrap in a function call so we know when the value is accessed). If the
template can be hacked, that's ideal --- otherwise we'd need to use a side
effect to broadcast an association between the generated content and the
sources (broadast would be an awful hack --- look for template mod
alternative).

Another, much much easier approach, would be to let a template denote that it
requires "data" in general --- i.e. put this problem on the user. The downside
is that the user has to deal with this. The upside is that it avoids a lot of
complexity and ever increasingly "magical" behavior.

## Error Messages

As much fun as it is to use Erlang, it's not fun to figure out what this means:

```
=== ERROR ===
{{template_compile,"/home/garrett/SCM/chicago-erlang-2014/templates/index.html",
                   {"/home/garrett/SCM/chicago-erlang-2014/templates/index.html",
                    [{102,erlydtl_scanner,"Illegal character in column 24"}]}},
 [{lpad_template,handle_compile,3,[{file,"src/lpad_template.erl"},{line,43}]},
  {lpad_template,render,3,[{file,"src/lpad_template.erl"},{line,28}]},
  {lpad,generate,2,[{file,"src/lpad.erl"},{line,175}]},
  {lpad,run,2,[{file,"src/lpad.erl"},{line,37}]},
  {erl_eval,local_func,5,[{file,"erl_eval.erl"},{line,544}]},
  {escript,interpret,4,[{file,"escript.erl"},{line,781}]},
  {escript,start,1,[{file,"escript.erl"},{line,276}]},
  {init,start_it,1,[]}]}
```

And another (will require that we generate an exception that has the template
source name, in addition to the other info):

```
=== ERROR ===
{{file_read,"hello.markdown",enoent},
 [{lpad_template_filters,handle_file_read,2,
      [{file,"src/lpad_template_filters.erl"},{line,72}]},
  {'/home/garrett/SCM/lambdapad/samples/hello/index.html',render_internal,2,
      []},
  {'/home/garrett/SCM/lambdapad/samples/hello/index.html',render,2,[]},
  {lpad_template,render,2,[{file,"src/lpad_template.erl"},{line,46}]},
  {lpad_template,render,3,[{file,"src/lpad_template.erl"},{line,29}]},
  {lpad,generate,2,[{file,"src/lpad.erl"},{line,178}]},
  {lpad,run,2,[{file,"src/lpad.erl"},{line,37}]},
  {erl_eval,local_func,5,[{file,"erl_eval.erl"},{line,544}]}]}
```

Refer to lpad_event:handle_error/1 for how to print errors non stupidly.

## DONE Export all in index.erl considered bad

What happens to unused functions? They sit in the index as litter.

We should have an option to lpad-gen (or an env var,
e.g. LPAD_INDEX_REQUIRE_EXPORT) that causes export_all to be dropped when
compiling the index.
