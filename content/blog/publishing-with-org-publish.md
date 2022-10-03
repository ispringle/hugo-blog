+++
title = "Publishing with org-publish"
author = ["Ian S. Pringle"]
tags = ["meta", "blog"]
draft = false
+++

While looking further into ox-hugo I decided to also look further into just
bog-standard org-publish. I already had some elisp to build out my org files to
html and I decided to break that elisp down into a literate program that is then
compiled into a build directory which contains all the shell, elisp, CSS, and
Dockerfiles I need to build and deploy a website built with org-publish. I am
not _currently_ using this to deploy this or any other website, but I was pretty
happy with the result and wanted to share. I think I could quickly get a website
that is almost identical to my current NextJS site.

You can check out [the build script]({{< relref "build" >}}) and see how I'm doing it.
