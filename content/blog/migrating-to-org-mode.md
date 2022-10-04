+++
title = "Migrating To Org Mode"
author = ["Ian S. Pringle"]
date = 2022-08-03T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

I am back to (doom)emacs+org-mode, again... again... again...

Why? I like it!

I tried to use ox-hugo. And it seemed nice, but way to much extra. Org mode can
compile org files to html, why convert the files to markdown first? Just didn't
make sense. ~~I think I shall try sticking with stock org-mode for publishing.~~

I ended up finding a wonderful package called [Uniorg](https://github.com/rasendubi/uniorg), which is a parser that
takes Org files and generates Unified ASTs that can then be manipulated by tools
like Rehype to modified and spit-out HTML. This removes the annoyance of ox-hugo
but means I am able to have a bit more control over my contnet -- or at least
_easier_ access to control -- than with org's own publishing feature. Though, I
did get the org-publish mostly working and producing acceptable HTML, I think.

Uniorg isn't the end-all-be-all, but it's fairly extensible and it is actively
maintained, unlike orgajs, which seems to have either died or at least has been
put on the back-burner by the maintainers. This does mean I have no need for
contentlayer, I have had the thought that maybe contentlayer could get org
compatibility added through the usage of Uniorg, however the more I think about
that the less sense it makes, org files are already much more structured and
defined than markdown with arbitrary frontmatter and Uniorg already is turning
the org files into data. For the mean time, I have gutted the contentlayer
aspects of [Sylvan](https://github.com/pard68/sylvan) and put Uniorg in. My next step with that project is going to
be figuring out how to build Sylvan with a remote repository for content, thus
meaning I can keep my org directory free of the Sylvan related project files and
vice versa.

A couple things I'd like to see out of Uniorg:

-   Expose drawers for gathering data from (I have managed to get `PROPERTIES`
     drawers, but none other, and the current solution for `PROPERTIES` drawers
    dumps all data from _all_ the drawers in a file into one, meaning if multiple
    headings have drawers than that data is getting combined and overwritten)
-   Respect the datatree
-   Less boilerplate

Other than that, I am fairly happy with Uniorg thus far. I will say, in
org-publish's favor, I liked the styling and layout of the default org-publish
function so much that I have endeavored to incorporate some of the asthetics
into my NextJS powered site.
