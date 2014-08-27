# What Does It Do?

Lambda Pad lets you create static content --- usually a website --- with
user-friendly, plain-text files like Markdown and JSON, along with templates
and other static content. It's like [Jekyll](http://jekyllrb.com) and
[Hyde](http://hyde.github.io), but it's in Erlang rather than Ruby or Python.

Static site generators are deceptively powerful. They can be used to implement
a huge range of applications including company web sites, blogs, presentations,
product documentation --- really, most content you run across on the Web can be
generated using this sort of tool!

For some thoughts on static site generation and why it's important,
[see this blog post from gar1t on Blogofile](http://www.gar1t.com/blog/blogofile.html).

# Why Another Tool?

It's fair to say that static site generation
[is a solved problem](http://staticsitegenerators.net). Why would anyone want
to created yet another static site generator when there are so many well
established, robust, proven tools already?

Because the world *needs* a static site generator that embraces *functional
thinking*. What does that mean?

- Behavior should be obvious, or as obvious as possible without excessive
  verbosity
- Users should not be stupefied and confused by implicit or otherwise magical
  behavior
- Solutions should be small and focused to make them easier to understand ---
  in general, the principle of
  [separation of concerns](http://en.wikipedia.org/wiki/Separation_of_concerns)
  should be evident when using the tool

When the goodness of functional thinking is applied to static site generation,
users will have more fun and be more productive!

# Why Erlang?

Erlang is a simple, easy-to-learn functional language that's incredibly
powerful. It's known for building highly reliable, massively scalable systems
(*shout-out to WhatsApp --- hey, what do you guys think of sponsoring a six
week Lambda Pad sprint in Fiji?*) but it's also terrific for tasks like site
generation. Erlang [term syntax][] is low-noise and expressive. Erlang
[functions][] are beautiful and maintainable.

Erlang is also [pretty dang fast][]!

[term syntax]: https://github.com/gar1t/lambdapad/blob/master/docs/index.erl#L7-L12

[functions]: https://github.com/gar1t/lambdapad/blob/master/docs/index.erl#L42-L43

[pretty dang fast]: http://stackoverflow.com/questions/6964392/speed-comparison-with-project-euler-c-vs-python-vs-erlang-vs-haskell

The question is not so much *Why Erlang*, but *Why Not Erlang*?

Most static site generators use so called "human friendly" languages for
configuration and customization. A few of the more popular options include
YAML, JSON, Ruby, and Python. Lambda Pad will show that Erlang --- yes,
straight up *Erlang* --- should be counted among the usual suspects for
ease-of-use, clarity, and productvity!

# Is Lambda Pad Ready for Prime Time?

Pretty much. It's quite functional (pun?) and is being used to generate a
couple of web sites:

- [chicagoerlang.com](http://www.chicagoerlang.com)
- [lambdapad.io](http://www.lambdapad.io)

It has some rough edges, which is to be expected at this stage. It's a young
creature and needs care and feeding. But watch out, it's gonna grow up soon!

For a rough cut list of To Do items, refer to the
[project TODO.md on GitHub](https://github.com/gar1t/lambdapad/blob/master/TODO.md).

# How Do I Start?

There are a couple ways to dive in:

- Check out the [Getting Started](/getting-started/) page on this site
- [Get Lambda Pad from GitHub](https://github.com/gar1t/lambdapad) and build
  the [samples](https://github.com/gar1t/lambdapad/tree/master/samples) and
  [docs](https://github.com/gar1t/lambdapad/tree/master/docs)
