+++
title = "Nice New Look"
author = ["Ian S. Pringle"]
date = 2022-09-10T11:58:00-05:00
tags = ["meta", "blog"]
draft = false
+++

You'll notice that my site has some nice new styles going on. I was playing with
a fork of Sylvan that used Chakra-UI, however I have since trashed that because
I couldn't get the layout I wanted (after giving up on the route I then updated
how my site is laid out and this ended up meaning that Chakra or Mantine or
really any other components library would have actually worked...). But that
sparked some renewed effort into fixing the styles. Initially I was just looking
for better heading font sizes and I found a way to do that, so then I started
playing with some other ideas, inspired by various sites I like the look of. I
think I will mostly stay with this current style, however I think I will also
implement some sort of "theme" engine eventually that will let me have loads of
themes, and each theme will then have a light and dark mode.

The current technical setup is okay -- and you can checkout how I'm doing it on
the Sylvan project repo -- but I'm not sure I am ultimately satisfied with just
having a light and dark mode. But at the same time... I really like this look
and don't want to ultimately trash it. I think my next effort will be to
differentiate between "themes" and "color scheme" so that I can then easily just
load in a different style. This will probably also involve me refactoring the
CSS into modules so as to avoid collisions and also will probably mean an easier
time interacting with the CSS via JS.

This current theme is giving me Pokemon vibes. I might try to implement my own
`border-image` with the little balls in the corners like in the original Pokemon
games. I've also been adding little easter eggs into the site just for fun,
might be worth keeping an eye out to see if you can find them all (gotta catch
'em all)!

And finally, I discovered through this that something is very wrong with either
NextJS, Vercel, or my GitHub action. Sylvan was building perfectly fine on my
machine but deploying to GH resulted in an error. Somewhere, something does not
like my usage of the `pages/api/` stuff. I have consigned to giving up on Vercel
and will eventually be moving my project to my own infrastructure. I have the
Dockerimage built and will do a little testing to make sure it's all good and
working, and then I'll flp the switch and say goodbye to Vercel. Likely this
will mean I'm moving my org repo, and perhaps even Sylvan to Gitlab because
honestly I hate GitHub Actions and have only ever experienced pain when trying
to build and deploy containers via GitHub Actions Likely this will mean I'm
moving my org repo, and perhaps even Sylvan to Gitlab because honestly I hate
GitHub Actions and have only ever experienced pain when trying to build and
deploy containers via GitHub Actions.
