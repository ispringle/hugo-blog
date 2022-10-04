+++
title = "Keeping Some Content Private A Stop-Gap"
author = ["Ian S. Pringle"]
date = 2022-08-08T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

So I'd like to keep _some_ content in my org repo private -- things like todos,
journals, and unfinished posts. Ultimately I'd like to put this sort of stuff
behind a login, but as a stop-gap I'm thinking to go the brute-force way and
`rm` it. My stop-gap is to delete any file containing the line `:PRIVATE: t`.
I'm going with the property route so that in the future I can update my solution
to be a bit more elegant without having to do any additional work to alter my
files.

So to make this work all I have to do is add the following to a step in my GitHub deployment action:

```shell
find . -type f -exec grep -q "^:PRIVATE: t\$" {} \; -delete
```

With this in place it is only a matter of annotating the files I don't want to
publish with the new property and they should cease to be deployed.

My ultimate goal is to have Uniorg change the slug for files annotated with this
property and prefix a `private` to the path or something along those lines. Then
I can setup Cloudflare to put a login prompt in front of any page that contains
this specific path. The key is going to be ensuring that whatever this path
alteration ends up being isn't something that I might accidentally use in some
other file -- say a blog post talking about private files...
