+++
title = "Deployment. How Do?"
author = ["Ian S. Pringle"]
date = 2022-08-07T00:00:00-05:00
tags = ["infra"]
draft = false
+++

## Introduction {#introduction}

Currently, [ian.ist](https://ian.ist) is hosted with [Vercel](https://vercel.com). I am using Vercel and not my
[Bitranchlabs Infra](https://gitlab.com/bitranchlabs/ops) because it was easier, and I am also not sure if I am going to
keep my Kubernetes stuff running -- it's $50+ / month for something I really
don't need. Vercel is great, I have no real reason to leave, except that my
website's _content_ is in my private org repo, while the rest of the site is in
the [Sylvan](https://github.com/pard68/sylvan) repo, and the org content must be mounted into the Sylvan repo in order
to build this site.

I might have a solution that would allow me to continue with Vercel, while also
having these two separate repos. Vercel can either build the project for you, or
it can be triggered from [GitHub actions](https://vercel.com/docs/concepts/git/vercel-for-github#using-github-actions). The best way I can think to use this is
to trigger the Vercel deployment from the private org-mode repo I have on Github.
This action will pull in my Sylvan repo, mount the org content into that project,
build the project, and then trigger the Vercel deployment. I think I can break this
into two approximate parts; first, trigger Vercel deployments from the org repo
and second trigger the org repo's action when changes occur to Sylvan.


## Deploying to Vercel from a GitHub action {#deploying-to-vercel-from-a-github-action}

I am just following right along with the docs they provide [here](https://vercel.com/support/articles/how-can-i-use-github-actions-with-vercel). Step one is to
create the workflow file in my org repo:

<a id="code-snippet--Github Action for deploying non-prod"></a>
```yaml
name: GitHub Actions Vercel Preview Deployment
env:
  VERCEL_ORG_ID: ${{ secrets.VERCEL_ORG_ID }}
  VERCEL_PROJECT_ID: ${{ secrets.VERCEL_PROJECT_ID }}
on:
  push:
    branches-ignore:
      - main
jobs:
  Deploy-Preview:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Vercel CLI
        run: npm install --global vercel@latest
      - name: Pull Vercel Environment Information
        run: vercel pull --yes --environment=preview --token=${{ secrets.VERCEL_TOKEN }}
      - name: Build Project Artifacts
        run: vercel build --token=${{ secrets.VERCEL_TOKEN }}
      - name: Deploy Project Artifacts to Vercel
        run: vercel deploy --prebuilt --token=${{ secrets.VERCEL_TOKEN }}
```

<a id="code-snippet--Github Action for deploying prod"></a>
```yaml
name: GitHub Actions Vercel Production Deployment
env:
  VERCEL_ORG_ID: ${{ secrets.VERCEL_ORG_ID }}
  VERCEL_PROJECT_ID: ${{ secrets.VERCEL_PROJECT_ID }}
on:
  push:
    branches:
      - main
jobs:
  Deploy-Production:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install Vercel CLI
        run: npm install --global vercel@latest
      - name: Pull Vercel Environment Information
        run: vercel pull --yes --environment=production --token=${{ secrets.VERCEL_TOKEN }}
      - name: Build Project Artifacts
        run: vercel build --prod --token=${{ secrets.VERCEL_TOKEN }}
      - name: Deploy Project Artifacts to Vercel
        run: vercel deploy --prebuilt --prod --token=${{ secrets.VERCEL_TOKEN }}
```

Of course, these alone won't do the trick since my org repo doesn't actually
contain a NextJS project. I need to _pull_ that NextJS code in from the Sylvan
repo in a separate step. We can do this by adding another checkout step to the
action:

```yaml
    - name: Checkout Sylvan
      uses: actions/checkout@v2
      with:
        repository: pard68/sylvan
        path: /app
```

I'll be honest, GitHub actions have always confused me, I'm never really certain
how they work or _where_ I am. I think the easiest way to do this is to actually
`checkout` the Sylvan repo _first_ and then `checkout` the org repo into a directory
of the previously checked out Sylvan repo, like such:

```yaml
jobs:
  Deploy-Production:
    runs-on: ubuntu-latest
    steps:
    - name: Checkout Sylvan
      uses: actions/checkout@v2
      with:
        repository: pard68/sylvan
    - name: Checkout Org content
      uses: actions/checkout@v2
      with:
        path: src/content/
```

UPDATE:

I ended up copying the org repo to a directory called `org/` and then using a
step to `mv org/* public/`. I did this because there is content in the `public/`
directory in the Sylvan repo that I want to keep (fonts and SVGs).


## Trigger builds when Sylvan repo changes {#trigger-builds-when-sylvan-repo-changes}

Now to trigger the build action when the Sylvan repo changes I need to add
another build hook called a `repository_dispatch`:

```yaml
on:
  push:
    branches:
      - master
  repository_dispatch:
    types:
      - build
```

I'm not going to do this on the preview builds, just doesn't seem like there's
much point to that, so we'll only trigger a rebuild on the master branch. This
dispatch event is basically a way for us to trigger the action from outside of
GitHub -- which is confusing because we're actually triggering this action from
_another_ GitHub repo's action. Think of this as a means of triggering actions
via REST. Next we create a new developer token with the `repo` scope and add
that to the repo that we want to "watch". And then finally we create the action
on that other repo. You'll want to update the repo name in the URI to
whatever user or org's repo is the one that is getting _triggered_. Also notice
that the `event_type` is set to `build`, which corresponds to the dispatch
`type` specified previously:

```yaml
name: Trigger rebuild of parent repo
on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  workflow_dispatch:

jobs:
  notify:
    runs-on: ubuntu-20.04
    container: alpine/httpie
    steps:
      - name: Notify parent repo
        run: http post https://api.github.com/repos/<User/Org>/<Repo Name>/dispatches "Authorization:token ${{ secrets.NOTIFY_TOKEN }}" event_type=build --ignore-stdin
```

I ended up changing the type from `build` to `sylvan-update` because that type
name is what the action's run is called and this way it's a bit more expressive


## Update! {#update}

Just a quick update, I have written my Github Action as a literate file, so now
you can check it out [here]({{< relref "#d41d8c" >}}), the nifty thing about this is that the file is what I
use to compile the yaml for the GH actions, so it is in some sense a "living"
document and will always reflect what I am actually using.
