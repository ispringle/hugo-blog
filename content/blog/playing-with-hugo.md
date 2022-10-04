+++
title = "Playing With Hugo"
author = ["Ian S. Pringle"]
date = 2022-08-23T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

So I've spent some time playing with Hugo and more specifically, I've been
playing with `ox-hugo` which is a way of exporting Org files to Markdown that
plays nicely with Hugo. I'm not happy with my current blog setup. I like the
Sylvan site and NextJS, but blogging with it is cumbersome, in part because of
the Org parser I am using. In JavaScript land there are two Org parsers, one is
mostly abondonware and the other is called Uniorg. It's great for what it is,
but it's not refined, it has a very poor way of interacting with it, it feels
fragile, and the author has expressed disinterest in support rather common and
crucial Orgmode features -- such as multiple drawers in a document,
non-PROPERTIES drawers (ie LOGBOOK), and subtrees.

In my opinion none of these are inherintly a deal breaker, and all are things I
could remedy by forking the project and building my own off of it. But, I'm not
interested in spending a huge time on this and if I did, why limit myself to
JavaScript? I see a few potential options:

1.  Use Hugo, it works out of the box and produces a very satisfactory website
    with nearly no fuss
2.  Fork Uniorg and add the myriad features I'd like to see
3.  Use another parser all together, such as [this python parser](https://orgparse.readthedocs.io/en/latest/), and then decide
    what to do about serving

Some things in ox-huge/hugo's favor:

-   it just works
-   converting my stylings to hugo shouldn't be too hard
-   ox-hugo supports a LOT of org features
-   files attached to org files are ported
-   it's static

Some things that I'm a little concerned about:

-   No server means less cool things
-   No node means harder to do cool JS things

I might flip the switch and if I do, you'll know...
