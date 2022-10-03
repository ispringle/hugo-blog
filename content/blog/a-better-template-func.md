+++
title = "A Better Template Func"
author = ["Ian S. Pringle"]
date = 2022-09-08T05:56:00-05:00
tags = ["emacs"]
draft = false
+++

In [a previous post]({{< relref "my-dirt-hack-to-template-files" >}}) I talked about a function I wrote that allowed me to stub out
a few of the properties required for working with Sylvan. I have since updated
that function a little. It's still not ideal and I have one or two issues with
it still, but I wanted to share the updated function nonetheless. As a sidenote,
unless I outright delete this function for some reason, it shall always be
visible in it's most up to date from in my [literate config]({{< relref "doom#blog-templates" >}}).

This function now prompts for a post title, turns that into a filename, creates
a buffer, writes out some initial data to that buffer, saves it to a file, and
then let's you continue writing it.

```elisp
(defun 0x44/create-new-blog-buffer ()
  "Created a new blog from the specified template in a new buffer"
  (interactive)
  (let* (($timestamp (format-time-string "<%Y-%m-%d %a %H:%M>" ))
         (title (read-from-minibuffer "Post Title: "))
         (fname (concat org-blog-directory "/" (org-hugo-slug title) ".org")))
    (let (($buf (generate-new-buffer title)))
      (switch-to-buffer $buf)
      (insert (format
               ":PROPERTIES:\n:AUTHOR: %s\n:CREATED: %s\n:MODIFIED: %s\n:TYPE: blog\n:END:\n#+title: %s"
               user-full-name $timestamp $timestamp title))
        (funcall 'org-mode)
        (funcall 'org-id-new)
        (setq buffer-offer-save t)
        (set-visited-file-name fname)
  $buf)))
```

You'll notice I am using a function from `ox-hugo`. This is literally the only
reason I have that package, so I'd like to get that dependency removed somehow
eventually.

I am not sure if `(set-visited-file-name)` is the appropriate function to use
here, but I have been unable to find _any_ other functions that will save the
buffer to a file and then ensure that the buffer is now set to that saved file.

This little function works very nicely and is a marked improvement on the
previous function.
