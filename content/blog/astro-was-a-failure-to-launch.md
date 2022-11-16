+++
title = "Astro Was A Failure To Launch"
author = ["Ian S. Pringle"]
date = 2022-10-06T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

I have spent the last couple days hacking away at AstroJS. I think it might provide what I want in a framework. It produces static sites with no, to minimal client-side JS, and it is itself a JS framework. SSG is important mostly at a philosophical level for me, Lightscores aside, the overhead of SSR or CSR for a blog is minimal and likely unnoticeable, but I enjoy the *idea* of SSG. I do really like that Astro makes a signficant effort to pre-render as much of the content as possible and deliver as little client-side rendering JS as possible, NextJS is sort of a joke in this regard even though it advertises as SSR it's really more like "Server-side-collected-client-side-rendered". You get a massive JSON file that the client then puts together with some JS helpers.

Being a JS framework is probably most important. I had this notion in mind when I set out to make this blog, and my brief use of Hugo reminded me of this. I actually could probably be very content with a non-JS framework if it had JS-level support for frontend technology. A statically generated blog is largely a backend beast, in fact by the time we're talking HTML it's really just a few directories and a server or reverse-proxy. It's *how* that HTML is generated that is the question and the best tooling for generating HTML+CSS+maybe-JavaScript is hands down the JS/Node ecosystem.

All of this to say, I tried Astro and it was a bit of a flop. Not entirely Astro's fault, I'm sure it's a great little framework that creates great results, however I never got that far because I couldn't coax it into producing anything with my Orgmode files. I attempted to build an Astro plugin to consume the `.org` files and convert it to *something* that Astro can handle. The real problem is in the *something* and also in the notion of "Astro plugin". The sense of being helpless, due to a distinct lack of most documentation on the subject and a very lackluster showing for the little documention that did exist, plus a rather non-existent community presence when it comes to the plugin world, did not help at all.

Here's the rub about plugins in Astro; they're only sort-of Astro plugins. They're actually Vite plugins with a slightly modified syntax for loading so that they get loaded by the Astro core instead of the Vite core. And here's the rub for Vite plugins; they're only sort-of Vite plugins. They're actually Rollup plugins with a slightly modified syntax for loading so that… you get the idea. This makes the documentation *fractured* and confusing. What do I return? A String? Well okay but what's the format of the string? HTML? No. JS! … that's ambiguous. Okay what sort of JS? A function? Wait… it's a string of JS…

Yes, I had a lot of headache. I gave up actually. The best I got was an error stating that I did not have a proper renderer to render my content and that I needed to install one. But the options were Vue, React, or Lit. The content was the string "Help me!". I'm unsure how any of those would render that…

And that's the *something* I was referring to earlier, what do I return exactly? It's a string that is JS, it seems sort of low effort, but I could be wrong, I'm not smart enough to write any of these tools.

At least for the time being I think I'll be sticking with NextJS, though I do plan to start implementing some changes to the UI because I sort of liked some of the things I ended up doing with Hugo from a aesthetics point-of-view.
