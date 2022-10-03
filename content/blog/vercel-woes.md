+++
title = "Vercel Woes"
author = ["Ian S. Pringle"]
tags = ["infra"]
draft = false
+++

Vercel is the company behind Nextjs and also a hosting platform that works with
static site generates (Huga, Jekyll, Zola, etc.) and server side generators
(Nextjs). I've been using them to host my new blog since it's inception because
they promise -- and mostly make good on the promise -- easy deployments. Well...
[ian.ist](https://ian.ist) is a little different because it's two different repos. There is the
[Sylvan](https://github.com/pard68/sylvan) repo which contains the Nextjs code that parses org files and displays
the content and there is my private org repo which holds the content. Because
I'm using a private repo I can't use Vercel's built in build process, and
because I'm trying to build Syvlan to be more of a framework than a personal
tool, even if Org was public it'd be hard/impossible to build the site with
Vercel's tooling. Luckily they have a path forwards with building and deploying
with Github Actions as well. I set this build process to run in the Org repo. I
have already detailed my build process in [Deployment. How Do?]({{< relref "deployment-how-do" >}}), but I am going to
just include the worflow file here as well, because it's changed a little since
then.

<a id="code-snippet--GH Action Workflow"></a>
```yaml
name: GitHub Actions Vercel Production Deployment

on:
  push:
    branches:
      - master
  repository_dispatch:
    types:
      - sylvan-update
  workflow_dispatch:

jobs:
  Deploy-Production:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout Sylvan
        uses: actions/checkout@v2
        with:
          repository: pard68/sylvan
      - name: "Remove the Sylvan .git dir"
        run: "rm -rf .git"
      - name: Checkout Org content
        uses: actions/checkout@v2
        with:
          path: org/
      - name: "Remove files marked :PRIVATE: t"
        run: 'find ./org -type f -exec grep -q "^:PRIVATE: t\$" {} \; -delete'
      - name: Move the org repo .git dir to root to satisfy Vercel
        run: mv org/.git ./
      - name: Merge org content with default Sylvan content
        run: mv org/* public/
      - name: Install Vercel CLI
        run: npm install --global vercel@latest
      - name: Pull Vercel Environment Information
        run: vercel pull --yes --environment=production --token=${{ secrets.VERCEL_TOKEN }}
      - name: Build Project Artifacts
        run: vercel build --prod --token=${{ secrets.VERCEL_TOKEN }}
      - name: Deploy Project Artifacts to Vercel
        run: vercel deploy --prebuilt --prod --token=${{ secrets.VERCEL_TOKEN }}
```

The changes are what this post is about.

The issue I was facing after the initial deployment was that my Vercel project
was being updated by changes to the Sylvan repo, which means that when those
changes got triggered, built, and then deployed I would end up with a blank
website, because there is no _content_ in the the Sylvan repo. My initial fix
was to disable Vercel from accessing my Github repos and that worked for a few
days but then I noticed that the GH Actions were starting to fail so I had to
come up with a new solution or get off Vercel.

My solution was based on a hunch. There was no reason that Vercel should know
about Sylvan. I never told it about Syvlan when I created the project. The only
way it could know Syvlan existed is if it was scraping the `.git/` when I
deployed via the Vercel CLI tool. So I followed my hunch, deleted that directory
and then replaced it with the Org repo's `.git/` directory.

SUCCESS! Almost.

Now it was building based on triggers from the _Org_ repo, which is better, but
those builds will _always_ fail because the org repo has no Nextjs code to
deploy! So then I removed the step to copy the org repo's git directory to the
root. The Github action actually failed this time, it seems that the Vercel CLI
requires the thing being uploads to use git, and without the `.git/` directory,
there is essentially no git.

Next idea: dummy git repo:

<a id="code-snippet--Dummy repo step"></a>
```yaml
      - name: Created a dummy git repo
        run: git init . && git commit -a -m "boop"
```

This didn't work because git didn't know my `user.email` or `user.name`. Before
I invested in bootstrapping git, I had had another idea; instead of creating a
dummy git, just remove the remote, which I assume is how Vercel is finding the
org repo.

<a id="code-snippet--Remove remote"></a>
```yaml
      - name: Move the org repo .git dir to root to satisfy Vercel
        run: mv org/.git ./
      - name: Expunge git remote
        run: git remote remove origin
```

The build and deploy worked! But when I pushed a new commit to Github Vercel
tried to build the project again, which of course failed. On a whim I checked
the Vercel project and it had somehow connected itself to the org repo. I
deleted the connection and... SUCCESS!

It's a little more work than I wanted, but I can keep using Vercel at least!
