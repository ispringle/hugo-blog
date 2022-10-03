+++
title = "My Dirty Hack to Template Files"
author = ["Ian S. Pringle"]
tags = ["emacs"]
draft = false
+++

It's actually kind of hard to use a template to create a new file. Apparently
this is part of Doom, but I couldn't figure out how to get teh `file-templates` to
work in Doom. My dirty hack to get this working was to create a function that
calls a new buffer, inserts some heading info into that new buffer, and then
tells the buffer use the major mode of org-mode.

```emacs-lisp
(defun 0x44/create-new-blog-buffer ()
  "Created a new blog from the specified template in a new buffer"
  (interactive)
  (let (($timestamp (format-time-string "<%Y-%m-%d %a %H:%M>" )))
    (let (($buf (generate-new-buffer "Untitled Blog Post")))
        (switch-to-buffer $buf)
        (insert (format ":PROPERTIES:\n:AUTHOR: %s\n:CREATED: %s\n:MODIFIED: %s\n:TYPE: blog\n:END:\n#+title: "
                user-full-name $timestamp $timestamp))
        (funcall 'org-mode)
        (setq buffer-offer-save t)
  $buf)))
```
