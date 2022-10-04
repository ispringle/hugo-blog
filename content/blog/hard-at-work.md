+++
title = "Hard At Work"
author = ["Ian S. Pringle"]
date = 2022-09-15T16:47:00-05:00
tags = ["meta", "blog"]
draft = false
+++

You might not know it since we're going on almost a week since a _content_ update
to the site, but I have been doing some refactoring work on the sylvan project.
I've cut build times in half by updating the build-api. An added benefit of this
rebuild is that I can now properly use rehype plugins (retext too!). Currently
I've got rehype-raw working which means I can add `begin-export html` blocks to my
org files and they will populate on the site as actual HTML (and I think react,
but I have not tried this yet so I do not know for sure). I've also been working
on a refactor of the styling, not the actual look but the code behind it. Some
stuff will be changing but it's because of the updates to the code and then me
making minor tweaks to the looks at the same time.

All this is being done in an effort to make future features easier to implement,
such as making Sylvan configurable and useable by others.
