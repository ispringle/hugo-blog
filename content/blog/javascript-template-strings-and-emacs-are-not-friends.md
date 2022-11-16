+++
title = "JavaScript Template Strings and Emacs Are Not Friends"
author = ["Ian S. Pringle"]
date = 2022-08-09T00:00:00-05:00
tags = ["meta", "blog"]
draft = false
+++

At the new job (Jack Henry) we're using a JavaScript framework called Lit. It's a Google project that makes working with the native Web Component API a bit less painful. Because Lit is merely some syntactic sugar around the native vanilla JS API for working with WCs it doesn't have fancy stuff like JSX (as oppossed to the other WC library, Stencil, which does). Instead Lit uses the native JavaScript template strings ie:

``` javascript
const name = "World";
const foo = html`
<div>
  <p>Hello ${name}</p>
</div>`;
```

This is all fine, except that in Emacs template strings are always treated as a strings and do not have syntax highlighting, auto-complete, auto-pairs, or even auto-indenting. Essentially, stock Emacs – or even Emacs with most/any JS mode enabled – will revert to Notepad level features when writing elements in Lit. "Kinda sucks" doesn't even begin to describe how annoying it is to write Lit components in Emacs.

I initially tried to get it working, gave up, sought out help online, found that others had this problem but no solution, and then downloaded WebStorm. I gave that a day and then tried Emacs again. This time I went to the Doom Discord and was able to determine that the ts-ls with lsp-mode could lend some help… it doesn't really, but web-mode does help a small bit. So now I have auto-complete, auto-pairs, and auto-indenting, but still no syntax highlighting, and you sort of get the feeling that the system is still somewhat broke. But atleast it makes Lit development in Emacs tolerable.
