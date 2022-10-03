+++
title = "What's Next For Sylvan"
author = ["Ian S. Pringle"]
date = 2022-09-02T16:51:00-05:00
tags = ["meta", "blog"]
draft = false
+++

For those who do not know [Sylvan](https://github.com/pard68/sylvan) is my NextJS app that org files and creates
webpages. It's not a static-site generator technically since it's doing
server-side rendering, but I currently do not have it setup so that the org
files are updated on the fly either (ie content only gets updated when a
deployment runs). Fixing this is on the roadmap, but first I want to get it
setup to be configurable so that I can, for example, change the header, the
favicon, the overall title, etc. from my org repo. This will do two things,
first and foremost it will mean Sylvan can be used by others without having to
fork the repo, and second it will mean I can make changes to my website without
changing the framework that manages the content. I haven't exactly figured out
how I plan to do this just yet. I think I will end up going with a `sylvan.org`
file that will have some K/Vs in a drawer and Sylvan can consume that at startup
via the Uniorg module and then from there the `<nav>` links and other things can
be updated.
