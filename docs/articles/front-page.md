# What Does It Do?

Lambda Pad lets you create static content --- usually for a website --- using
user friendly data files like Markdown and JSON, along with templates and other
static content. It's like [Jekyll](http://jekyllrb.com) and
[Hyde](http://hyde.github.io), but it's in Erlang rather than Ruby or Python.

Static site generators are deceptively powerful. They can be used to implement
a huge range of applications including company web sites, blogs, product
documentation --- really, most content you run across on the Web can be
generated using this sort of tool!

For some thoughts on static site generation and why it's important,
[see this blog post from gar1t on Blogofile](http://www.gar1t.com/blog/blogofile.html).

# Why Another Tool?

It's fair to say that static site generation
[is a solved problem](http://staticsitegenerators.net). Why would anyone want
to created yet another static site generator when there are so many well
established, robust, proven tools already?

Simple. Because it's fun!

There are of course some specific motivations. Lambda Pad wants to bring some
"functional thinking" to the domain of static site generation. What does that
mean?

- Behavior should be obvious, or as obvious as possible without bludgeoning
  users with verbosity  
- Users should not be stupified and confused by implicit or otherwise magical
  behavior
- Solutions should be small and focused to make them easier to understand ---
  in general, the principle of
  [separation of concerns](http://en.wikipedia.org/wiki/Separation_of_concerns)
  should be evident when using the tool

Naturally, these are just silly ideas --- in the final analysis, Lambda Pad
should be fun and productive to use!

# Why Erlang?

Erlang is a terrific and fun functional language. Lambda Pad is a project that
encourages users who don't know anything about Erlang to learn a little at a
time while solving fun problems like how to setup a blog.

Most static site generators use so called "human friendly" languages for
configuration and customization. A few of the more popular options include
YAML, JSON, Ruby, and Python.

Why not Erlang? What's so obviously easier and friendlier about these other
languages that make them them automatic options for so many people?

Lamda Pad will at least *attempt* to use Erlang --- yes, straight up *Erlang*
--- in the same way that other static site generators use YAML, Ruby, etc.

# Is Lambda Pad Ready for Prime Time?

Yes, sort of. It's quite functional and is being used to generate a couple of
web sites:

- [chicagoerlang.com](http://www.chicagoerlang.com)
- [lambdapad.io](http://www.lambdapad.io)

But it has some rough edges, which is to be expected at this stage. It's a
young creature and needs some time to grow.

For a rough cut list of To Do items, refer to
[the project TODO.md file on GitHub](https://github.com/gar1t/lambdapad/blob/master/TODO.md).

# How Do I Start?

There are a couple ways to dive in:

- Check out the [Getting Started](/getting-started/) page on this site
- [Get Lambda Pad from GitHub](https://github.com/gar1t/lambdapad) and build
  the [samples](https://github.com/gar1t/lambdapad/tree/master/samples) and
  [docs](https://github.com/gar1t/lambdapad/tree/master/docs)
