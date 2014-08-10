id     : first-impressions
title  : My First Impressions Of LambdaPad
date   : 2014-08-09
author : Stan The Martian
blurb  : How the Erlang based site generator strikes my fancy.

Having used LambdaPad for my awesome blog, here is my first impression.

The good:

- It's explicit - very little magic
- Great [Django template](https://docs.djangoproject.com/en/dev/ref/templates/)
  support
- Great [Markdown](http://daringfireball.net/projects/markdown/) support with
  [multimarkdown](http://fletcherpenney.net/multimarkdown/)
- Seems pretty fast, with nice "re-generate only what's changed" (some bugs
  though)
- Believe it or not, the Erlang site definition is actually pretty damn easy to
  read and maintain

The less-than-good:

- Documentation is pretty much non-existent
- Bug with re-generating content depending on template ``extends`` and
  ``includes`` directives
- Would be nice to have a "watch" feature where the site is automatically
  updated as underlying sources are changed
- Not sure how I can include content from within a markdown source (Sphinx has
  this feature and it's indispensable)

## Being Explicit

I've worked a lot with [Hyde](http://hyde.github.io) --- it's a terrific static
site generator written in Python. However, I'm routinely confused and surprised
by what it does and have I have a hard time tracking down answers. It uses a
lot of cleverness, which may be powerful in one respect, but it's also hard to
follow.

LambdaPad has [*one* configuration file](../examples/index.erl.html), which
defines the site structure and the data that feeds the generators.

## Django Templates

The
[Django template language](https://docs.djangoproject.com/en/dev/ref/templates/)
is remarkably useful for defining site content. Just take a look at the
templates used for this blog! With the ``extends`` tag you can defined
base-level templates that provide overall site look-and-feel and structure and
then customize each page as needed within that structure. With the ``include``
tag you can include template snippets. This is great for frequently used
content that doesn't appear in a base template --- e.g. the "Recent Posts"
section listed on this page an include.

## Multimarkdown

Multimarkdown is a very functional and fast markdown processor. Check out
[the feature list](http://fletcherpenney.net/multimarkdown/features/) --- it's
all available (I think, well, at least the Markdown-to-HTML support) in
LambdaPad!

## Smart Content Generation

LambdaPad keeps track of sources that effect generated content. For the most
part, this works very well --- when you rebuild a site, content is generated
only when the underlying sources have changed. This is pretty damn nice. And
fast!

LambdaPad does have a bug where it fails to track templates that are included
or inherited from. So if you change a base template or an included snippet, you
won't get the right content re-generated. You can work around this of course by
manually deleting the effected targets, but this is a pain and this bug should
be fixed.

## Erlang Syntax

Check out [the site definition for this blog](../examples/index.erl.html) and
judge for yourself!

## Things To Look Forward To

LambdaPad is pretty function today, but it still has a ways to go. There are
some bugs but those will get fixed if the Calculus has anything to say about
it. Documentation will be a major effort, but that will fall into place (it's
pretty easy already to write a blog in LambdaPad --- docs can't be that far
behind).

I can imagine a feature where LambdaPad runs in a continual "watch" mode and
just re-generates content immediately when something changes. This would be
uber productive and stupendous and great.

From the look of my own ``index.erl`` for this blog, I wonder if some of the
syntax could possibly be improved. I know that functions can be used to express
things clearly --- and I've tried to do that --- but I wonder how this will
look to me over time. And when I'm drinking.

Keep an eye out for complexity and weirdness. This approach to site generation
could go horribly wrong if the LP authors aren't careful. Keep it simple --- as
simple as possible!
