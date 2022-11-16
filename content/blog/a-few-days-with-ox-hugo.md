+++
title = "A Few Days With ox-hugo"
author = ["Ian S. Pringle"]
date = 2022-10-04T05:28:00-05:00
tags = ["meta", "blog"]
draft = false
+++

I've been playing with ox-hugo for a few days now. I do like a lot about it. One
thing I like a lot is how it is focused on getting out of the way and just
turning org files into well crafted HTML. I am rather unpleased with ox-hugo's
path for deployment, it's a bit more manual than I like. I tried to automate it
but hit a roadblock. I'll definitely be trying this again later because the
current solution isn't acceptable to me. This is going to sound weird, but
ox-hugo expects you to *know* all the files you want to publish, whereas Sylvan
checks them for you and says "oh this is publishable and this is not." That
means that you make the conscience effort to say "publish this" but in the
future you do not need to recall back to what was and what wasn't published.

I also dislike how unorganized the Hugo project feels. This alone is keeping me
from committing to ox-hugo/Hugo. I'm somewhat displeased with Sylvan, but I
can't put my finger on the why. Part of it is because at the moment, Sylvan is
wholly un-reusable. It is entirely mine and while that's *okay* it was not what
I was setting out to build. At some future point I may come to address this
issue, but I am going to set it aside, because for the time being, if I'm not
happy with Sylvan, no one will be, so there is little point in trying to build
it in that direction.

A positive outcome from the exploration into Hugo is that I had a chance to look
at some stylistic things and layout ideas. I think *my* Sylvan is currently a
bit on the unreadable side and I will address that before too long. One think I
might look at in a future iteration of Sylvan… a Sylvan 1.0 – since this is very
much a 0.x build – is using Astro to create a static site. There is no reason
why Sylvan must be SSR'd, it could very easily be setup to use a SSG and I think
I will pursue this before I start looking at anything else – other than styling
and readability.
