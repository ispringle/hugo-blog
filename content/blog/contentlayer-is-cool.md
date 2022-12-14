+++
title = "ContentLayer Is Cool"
author = ["Ian S. Pringle"]
date = 2022-07-27T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

I've been using [contentlayer](https://contentlayer.dev/) with this new NextJS
blog/digital-garden/website/thing and so far I'm really liking it. Content Layer
is basically a way of reading MD and MDX files and then turning them into JSON
that JavaScript can then process. In addition to reading markdown into data,
Content Layer also does a few other things that other libraries aren't offering.
With Content Layer I can not only turn MD into data, but it also creates
TypeScript typing for that data and allows you to extrapolate and generate
metadata on the fly to go with that JSONified markdown.

For example, on this blog I currently have two "types" of content; `BlogPost` and
`BookReview`. A `BlogPost` is any file found in my `sylvan/content/blog` directory and
it will be accessible from the `/blog/` endpoint. Likewise `BookReview` content is
found in `sylvan/content/book`. Each content type has it's own iterate that you
can import to loop/map/find/etc. over and is also located in the `allDocuments`
iterator.

Where Content Layer is going to really shine is when I figure out how to hook
`sylvan` up to my knowledgebase and then exclude some files from being generated
into content -- for example maybe I don't want to import my work-related
documents into my personal website.

In the future I'd like to look into what I can do with MDX files. If I continue
on with using Obsidian as a CMS, I will probably not have too much use for MDX
files, but maybe for very technical or JavaScript/React related files the
benefits of MDX will outweight the announce of using MDX in Obsidian. But then
again, I am not entirely sold on Obsidian and am looking at getting
[planwarrioir](https://github.com/pard68/planwarrior), which would remove my current _big_ use of Obsidian -- the Day
Planner plugin.
