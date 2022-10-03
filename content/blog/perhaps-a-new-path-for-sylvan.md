+++
title = "Perhaps A New Path For Sylvan"
author = ["Ian S. Pringle"]
date = 2022-09-26T17:14:00-05:00
tags = ["meta", "blog"]
draft = false
+++

To me the idea of "sylvan" is a tool meant to easily create beautiful sites with
org-mode, while remaining unintrusive. For that reason, I am exploring some
alternative, non-NextJS options for Sylvan. There's a few reasons for this:

1.  As I've expressed before, I'm not 100% thrilled with Sylvan as it exists
    today
2.  As I've expressed before, I'd love for a more emacs-centric tooling, but I
    dislike the constraints of `ox-html`
3.  I am taking a new job on October 10th and my primary frontend framework will
    be lit.dev, and I think there could be a far more interesting future for
    Sylvan in the world of web-components
4.  NextJS can produce static sites, but it's very difficult to coax it into that
    box, and I'd prefer a site that's statically compiled

So to this end, I am experimenting with new options for the _idea_ of Sylvan. My
current experiments involve augmenting `ox-html` by deriving a new export backend
for org-mode from it. The current working theory is that any element can be
templated, if desired, and if it is templated than Sylvan will use that template
file as its definition for that org element. Any elements not defined will
fallback to the `ox-html` element. I'm not sure if this is even possible, but I'm
going to give it a shot and see what I can do!
