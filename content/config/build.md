+++
title = "Build"
author = ["Ian S. Pringle"]
draft = false
+++

## Introduction {#introduction}

This is my org-publish build script. I was just writing directly to the `build.el`
file, but then I started thinking about it, and that made no sense. If I'm using
_Orgmode_ to write a blog, I should be _using_ it to write the thing that writes
that thing that builds the blog! It's elementary!

Why a `build.el` and not just include this in my emacs config so that it all
automatically runs when I publish through emacs? Because my workflow doesn't
include me publishing via emacs. Instead I `git add . && git commit -m "Updates!"
&& git push` and then expect that my CI/CD will build and deploy my website for
me. Because of this expectation, I'd rather have the org-publish related stuff
separate from my emacs configuration so that the CI/CD has a much easier time
setting up emacs. Plus, when I do want to build the site locally I just have to
run `sh build.sh` which then will trigger `build.el`.


### Resources {#resources}

-   <https://gitlab.com/ngm/commonplace/-/blob/master/publish.el>
-   <https://commonplace.doubleloop.net/how-i-publish-my-wiki-with-org-publish>
-   <https://www.ereslibre.es/blog/2019/09/building-a-personal-website-with-org-mode.html>
-   <https://www.gonsie.com/blorg/ox-hook.html>
-   <https://orgmode.org/manual/Advanced-Export-Configuration.html>
-


### Alternatives {#alternatives}

-   <https://theiceshelf.com/firn.html>
-   <https://emacs.love/weblorg/>
-   <https://github.com/org-roam/org-roam-ui/discussions/109>
-   <https://ox-hugo.scripter.co/>
-   <https://git.sr.ht/~jakob/ox-haunt>


## README.org {#readme-dot-org}

It seems a little weird to build an org file, with an org file. I'm really only
doing this because the `build` directory is entry built from this org file, but
the intention is that I compile the files in `build` locally and commit them.
There is a chance that someone -- including future me -- might come upon this
`build` directory and be uninformed about it's creation and use.

```org
#+title: Readme

NOTICE: This file and all files in this directory were built with =build.org=
and should not be directly edited!

This is the build dir for use with org-publish. It should work, but I'm not
using it at the moment. Should just have to run `./build.sh` and it will spit
out the org files in the parent directory into a `./_html/` dir.

To use the Dockerfile, you'll need to be in the parent directory, aka ~org/~,
and then you will run =docker build -t test -f build/Dockerfile .=

The final nginx container has the http port exposed.
```


## build.sh {#build-dot-sh}

This is just a little shell script we're using to make it easy to call the elisp
code that does the real work of building the website.

<a id="code-snippet--build.sh"></a>
```bash
rm -rf ./_html/
emacs -Q --script build.el
```


## build.el {#build-dot-el}

Now onto the main attraction, `build.el` will do all the heavy lifting of building
the website.


### Front Matter {#front-matter}

Just a little front matter to describe the software, probably pointless really,
but it's here just in-case. I'll probably eventually fill in the commentary and
description...

```elisp
;;; build.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ian S. Pringle
;;
;; Author: Ian S. Pringle <pard@0x44.pw>
;; Maintainer: Ian S. Pringle <pard@0x44.pw>
;; Created: August 02, 2022
;; Modified: August 24, 2022
;; Version: 0.2.0
;; Keywords: bib convenience docs files hypermedia lisp outlines processes tools
;; Homepage: https://github.com/pard68/org
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
```


### Make `sh-set-shell` quiet {#make-sh-set-shell-quiet}

The `sh-set-shell` gets called on files when there is shell scripts in it. I'm not sure what it does but it is very noisy and I dislike the noise. So we can inhibit it with this:

```elisp
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))
```


### Dependencies {#dependencies}

We need to be able to install some dependencies, since we can't count on the
`emacs.d` directory having them installed already during CI/CD, plus we can also
separate this from our packages we use for normal, everyday emacs, which means
we can depend on different versions or even on things we don't want polluting
the rest of our setup.


#### straight.el {#straight-dot-el}

I'm messing around with using `straight.el` in addition to `use-package` because the
package `ox-attach-publish` is not on Melpa or any other package repo currently.
If this works well, I will work on refactoring the above dependencies to use
`straight.el` instead of `package.el`.

This will bootstrap `straight.el`, I got it _straight_ from their git repo:

```elisp
(setq package-enable-at-startup nil)

(setq straight-build-dir (expand-file-name "./.packages"))
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
```


#### Getting And Requiring Packages {#getting-and-requiring-packages}

Now that we can `use-package` we can get on with, well, _using_ packages. Let's
start with `org` and `htmlize`. Org is... well it's org! and `htmlize` is more or less
`pygments` or `highlight.js`, but for `org-publish`.

```elisp
;; Install needed packages
(use-package org)
(use-package htmlize)
(require 'ox-publish)
(require 'ox-html)
(require 'htmlize)
```

`s` is a package for handling strings, `f` is a package for working with files. I'm
not actually using either right now, I just am leaving this here (untangled) as
a reminder to myself to invest some time into making use of it at a future date.

```elisp
(use-package s)
(use-package f)
(require 's)
(require 'f)
```

`ox-attach-publish` is a tool that theoretically converts attached files into
valid links for orgmode to then use for images and such. But I haven't gotten
this working yet so it's not being tangled into my build script.

```elisp
(use-package ox-attach-publish
  :straight '(ox-attach-publish
              :type git
              :host github
              :repo "simoninireland/ox-attach-publish"))
(require 'ox-attach-publish)
```

We require `org-roam` to build and parse the `org-roam` related files I have.
Specifically, I'm looking to use this to generate the links and backlinks
between those files.

```elisp
(use-package org-roam)
(require 'org-roam)
(require 'org-roam-export)
```

Finally, we'll `require` everything we need, including some things that we didn't have to download first:

```elisp
(require 'find-lisp)
```


### The Meat and Potatoes {#the-meat-and-potatoes}

This is ephemeral, we don't need no stinking backups!

```elisp
(setq make-backup-files nil)
```


#### Common Variables {#common-variables}

Now to setup some variables for later use:

```elisp
(defvar build--build-dir (getenv "PWD"))
(defvar build--project-dir (concat build--build-dir "/.."))
(defvar build-publish-dir (concat build--build-dir "/_html"))
(defvar build--site-name "ian.ist")
(defvar build--publish-url "https://ian.ist")
```


#### Initialize org-roam {#initialize-org-roam}

We initialize the org-roam project and DB so that we can lean on it later to
generate backlinks.

```elisp
(setq org-roam-directory build--project-dir
      org-roam-db-location (concat build--project-dir "/org-roam.db"))

(org-roam-update-org-id-locations)
```


#### &lt;head&gt; {#head}

Here we turn off inlining CSS and then inject our own CSS into the `<head>`

```elisp
(defvar build--html-head
  (concat
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\" />"
   "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\" />"
   "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=VT323&family=Didact+Gothic\">"))
```


#### Some macros {#some-macros}

```elisp
(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">$1</span>@@")))
```


#### Sitemap Maker {#sitemap-maker}

We'll use this later to setup our `/{dir}/index.html` pages, for example to list
all blog posts:

```elisp
(defun build--org-sitemap-date-entry-format (entry _ project)
  "Build sitemap/index for a number of pages.
Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that
includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
                                  (org-publish-find-date entry project))
              entry
              filename))))
```


#### CSS Inliner {#css-inliner}

This inlines CSS, but I'm not using it right now. Eventually I'd like to pursue
optimizations that would allow for in-lining critical CSS and then throwing the
rest into the `styles.css`.

```elisp
(defun my-org-inline-css-hook (exporter)
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle (concat build--build-dir "/style.css") path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)
```


#### Navbar / Header {#navbar-header}

We'll use this later on as the preamble for every page:

```elisp
(defvar build--nav-bar "<nav><a href=\"/index.html\">/index</a>
                |
                <a href=\"/about.html\">/about</a>
                |
                <a href=\"/blog/index.html\">/blog</a>
                |
                <a href=\"/loci/loci.html\">/loci</a>")
(defvar build--logo
  (concat "<pre id=\"logo\">"
          (shell-command-to-string (concat "figlet " build--site-name))
          "</pre>"))

(defvar build--header (concat build--logo build--nav-bar))
```


#### Footer {#footer}

This is the footer or postamble:

```elisp
(defvar build--footer-left "<div id=\"footer-left\">
<p class=\"author\">Author: %a</p>
<p class=\"date\">Site Updated: %T</p>
<p class=\"creator\">Created with ❤️ & %c</p>
</div>")

(defvar build--footer-mid "<div id=\"footer-mid\">
<img class\"fleuron\" src=\"/fleuron.svg\" />
</div>")

(defvar build--footer-right "<div id=\"footer-right\">
<p class=\"copyright-notice\">Creative Commons</p>
<a href=\"http://creativecommons.org/licenses/by-nc-sa/4.0/\">BY-NC-SA</a>
</div>")

(defvar build--footer (concat build--footer-left build--footer-mid build--footer-right))
```


### org-publish projects {#org-publish-projects}

We'll now build out the projects. Each "project" is a like a group of pages. So
there is a "blog" project for all the stuff under `blog` directory, for example.

```elisp
(setq org-html-validation-link nil)
(setq org-publish-project-alist
      (list
```


#### "pages" project {#pages-project}

```elisp
       (list "pages"
             :base-directory build--project-dir
             :publishing-directory build-publish-dir
             :publishing-function 'org-html-publish-to-html
             :html-head-extra build--html-head
             :html-preamble build--header
             :html-postamble build--footer
             :html-head-include-default-style nil
             :recursive nil
             :with-author t
             :with-creator t
             :with-drawer t
             :with-toc t
             :section-numbers nil)
```


#### "loci" project {#loci-project}

```elisp
       (list "loci"
             :base-directory (concat build--project-dir "/loci")
             :publishing-directory (concat build-publish-dir "/loci")
             :publishing-function 'org-html-publish-to-html
             :html-head-extra build--html-head
             :html-preamble build--header
             :html-postamble build--footer
             :html-head-include-default-style nil
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-format-entry 'build--org-sitemap-date-entry-format
             :sitemap-sort-files 'alphabetically
             :with-author t
             :with-creator t
             :with-drawer t
             :with-toc t
             :section-numbers nil)
```


#### "blog" project {#blog-project}

This contains all the files in the `blog` dir. I eventually would like to figure
out how to make this work with a single `blog.org` file and then each heading or
maybe subheading in that file is a "page" on the site.

```elisp
       (list "blog"
             :base-directory (concat build--project-dir "/blog")
             :publishing-directory (concat build-publish-dir "/blog")
             :publishing-function 'org-html-publish-to-html
             :html-head-extra build--html-head
             :html-preamble build--header
             :html-postamble build--footer
             :html-head-include-default-style nil
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-format-entry 'build--org-sitemap-date-entry-format
             :sitemap-sort-files 'anti-chronologically
             :with-author t
             :with-creator t
             :with-drawer t
             :with-toc t
             :section-numbers nil)
```


#### "static" project {#static-project}

This contains all the static content in my `org` directory.

```elisp
       (list "static"
             :base-directory "~/org/"
             :base-extension "txt\\|jpg\\|jpeg\\|png\\|svg\\|gif\\|js"
             :recursive t
             :publishing-directory build-publish-dir
             :publishing-function 'org-publish-attachment)
```


#### "assets" project {#assets-project}

This contains all the assets in my `org/build` directory.

```elisp
       (list "assets"
             :base-directory "~/org/build"
             :base-extension "css\\|js\\|svg"
             :recursive nil
             :publishing-directory build-publish-dir
             :publishing-function 'org-publish-attachment)
```


#### "ian.ist" project {#ian-dot-ist-project}

This is just a "meta" project that contains all the above projects as
components:

```elisp
       (list "ian.ist"
             :components (list "pages" "loci" "blog" "static" "assets")
             :auto-sitemap t
             :sitemap-filename "sitemap.org"
             :html-doctype "html5"
             :html-html5-fancy t)))
```


### org-publish {#org-publish}

And finally, we build the project!

```elisp
(org-publish "ian.ist" t)
(message "Build completed!")
(provide 'build)
;;; build.el ends here
```


## style.css {#style-dot-css}

```css
html,
body {
  height: 100%;
  width: 100%;
}

html {
  font-family: "Didact Gothic";
  text-rendering: geometricPrecision;
  -webkit-font-smoothing: antialiased;
}

body {
  display: flex;
  flex-direction: column;
  margin: unset;
}

#content {
  flex: 1 0 auto;
  margin: auto;
  max-width: min(669px, 60vw);
}

#preamble {
  display: flex;
  flex-direction: column;
  align-items: center;
  margin: unset;
}

#preamble.status {
  margin: unset;
}

#preamble nav {
  font-size: 2em;
}

#preamble #logo {
  background-color: white;
  border: unset;
  font-size: 1.25em;
  padding: unset;
  width: fit-content;
}

#postamble {
  display: flex;
  flex-direction: row;
  align-items: center;
  width: 100%;
  justify-content: space-between;
  font-size: 0.7em;
  line-height: 1em;
}

#postamble * {
  margin: 5px;
}

#postamble p {
  margin: unset;
}

#postamble div:first-child,
#postamble div:last-child {
  display: flex;
  flex-direction: column;
  flex: 1;
}

#postamble div:last-child * {
  align-self: flex-end;
}

#postamble div:nth-child(2) {
  flex: 0 0 auto;
}

```


## fleuron.svg {#fleuron-dot-svg}

Since SVGs are just XML, I can document my favicon/fleuron in the build doc
here, it'll generate and be exported when I tangle the doc. Pretty neat!

```xml
<svg xmlns="http://www.w3.org/2000/svg" xml:space="preserve" width="20"
height="20" fill-rule="evenodd" clip-rule="evenodd" image-rendering="optimizeQuality"
shape-rendering="geometricPrecision" text-rendering="geometricPrecision"> <path
    d="M9 0v1h-.5v2H8V2H7v-.5h-.5V1H6V.5H3.5V1H3v.5h-.5V2H2v.5h-.5V3H1v1H0v2.5h.5V7h1v-.5H2V6h1v-.5h-.5V5H2V4h-.5v-.5H2V3h.5v-.5H3V2h1v-.5h.5V1H5v1h1v.5h1.5v1H8V4h1v.5h1V5h1.5v1H11v1h-1v1h.5v1h.5v.5h-.5v.5H10V8.5h-.5V8H9v-.5H8V7h-.5v-.5H6V7h-.5v.5h-1v1h-1V10H3v3.5h.5v2h1v1H5v.5h.5v.5H6v.5h.5v.5H7v.5h1v.5h2.5v.5h3v-.5H15V19h.5v-.5h.5V18h.5v-.5h.5V17h-1v.5h-.5v-1H15v.5h-.5v.5H14v1h-.5v.5H13v.5h-2V19h-.5v-2h.5v-.5h1V16h1v-.5h1.5V15h.5v-.5h.5V14h.5v-.5h.5V12h.5v-.5h.5V9H17v-.5h-.5V8H16v-.5h-3.5V8H12v.5h-1v-1h.5v-1h.5v-1h1V6h.5v.5h2V6h.5v-.5h1V5h1V4h.5v-.5h.5V3h.5v-.5h.5V2h-.5v-.5H19V1h-.5V.5H18V0h-.5v.5H17V1h-1v.5h-.5V2h.5v.5h.5V3h1v.5H17V4h.5v.5h-1V5h-1v.5H14V5h-2V4h.5V2.5H12V1h-1V0H9.5zm6.5 16.5h.5V16h-.5ZM10 .5h.5v1h.5V2h.5v.5H11v1h.5v1h-1V4H10v-.5h-.5V3H9V2h.5V1h.5Z">
</path> </svg>
```


## Dockerfile {#dockerfile}

This is the Dockerfile that will run the `build.el` build script, and then put
that into an nginx container for hosting or testing. Should be noted that this
needs to run from the parent directory to `build`, in this particular case that
means in `~/org/`..

Start by grabbing [silex/emacs](https://github.com/Silex/docker-emacs) and name it 'builder'.

```dockerfile
FROM silex/emacs AS builder
```

Make the working directory `/org`. Then make the `emacs.d` directory, this is mostly
useless but it ensures it exists when we install stuff with our `use-package` in
`build.el`. Then we just add some extra dependencies. I am not sure if I even need
`build-essential`... we need `sqlite3` to build the `org-roam` database. And
`git-restorem-time` is currently unused but I think I will eventually make use of
it and so I'm just leaving it here as a reminder:

```dockerfile
WORKDIR /org
RUN mkdir -p ~/.emacs.d/private/ && apt-get update && apt-get --yes install build-essential sqlite3 git-restore-mtime
```

Next, copy the entire `org` directory to the working directory, `cd` into `build/`
and kick off the `build.sh` script:

```dockerfile
COPY .. .
run cd ./build/ && ./build.sh
```

Finamly, create an nginx container named 'server', copy the statically compiled
assets to it, and then annotate port 80 as the port to use.

```dockerfile
from nginx as server
copy --from=builder /org/build/_html/ /usr/share/nginx/html/
expose 80
```
