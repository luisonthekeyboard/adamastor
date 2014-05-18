adamastor 0.2.0
===============

Destroying caravels [since the 1400s](http://en.wikipedia.org/wiki/Adamastor)...

## Some design constraints

I'm trying to follow [the original](https://daringfireball.net/projects/markdown/syntax) Markdown Syntax as much as possible,
with some deviations going in the direction of the [Vanilla Flavoured Markdown](http://vfmd.github.io/vfmd-spec/syntax/)
and others more in the direction of the [Pandoc Markdown](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown).

While this may sound like a spaghetti decision, actually there's a rationale for it:

> Try to be as explicit about what you're writing as possible.

So as an example, take the common blockquote inside blockquote example:

        > This is a blockquote
        > > with a child

If you write it like that, Adamastor will give you back something like

        <blockquote>
            <p>This is a blockquote &gt; with a child</p>
        </blockquote>

Which means that if what you wanted to get was

        <blockquote>
            <p>This is a blockquote</p>
            <blockquote>
                <p>with a child</p>
            </blockquote>
        </blockquote>

Then there should have been an explicit line between those two lines, like this:

        > This is a blockquote
        >
        > > with a child

There's probably more cases like this but you get the idea. John Gruber [seems to be in favour of this](http://article.gmane.org/gmane.text.markdown.general/2146)
as well and my own opinion is that it never hurts to be explicit.
