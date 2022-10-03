+++
title = "The Time Is Wrong!"
author = ["Ian S. Pringle"]
date = 2022-08-18T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

Well... it has happened again. The last time it was because of the YAML format
interpretting timestamps as UTC unless explicitly set otherwise. And I [fixed that
issue]({{< relref "the-case-of-the-wrong-date" >}})... but that won't help this, since I gave up on using Contentlayer like
two days later 😭.

I'm not sure the reason for the problem this time. I even [updated](https://github.com/szenius/set-timezone) the timezone
for the GH action so that it would be set to CST. Nonetheless, the date entries
in the indices (anywhere that is listing blog posts or content) is a whole day
_earlier_ than the actual timestamp for the entry.
