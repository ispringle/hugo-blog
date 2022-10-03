+++
title = "Github Workflow"
author = ["Ian S. Pringle"]
draft = false
+++

This is my Github workflow. This file is a literate file that uses org-tangle to
"compile" the needed yaml for Github.


## Workflows {#workflows}


### publish.yaml {#publish-dot-yaml}


#### Triggers {#triggers}

Step one, we declare the workflow, and when it can be triggered.

In this case, the workflow triggers on changes to the `master` branch:

```yaml
name: GitHub Actions Vercel Production Deployment

on:
  push:
    branches:
      - master
```

We also are going to trigger when the upstream NextJS project, [Sylvan](https://github.com/pard68/sylvan), is
updated, so we added a `repository_dispatch` trigger:

```yaml
  repository_dispatch:
    types:
      - sylvan-update
```

And lastly, let's add the `workflow_dispatch` trigger so we can kick off the build
manually:

```yaml
  workflow_dispatch:
```


#### Jobs {#jobs}

Next we declare the jobs, in this case there is only one, `Deploy-Production`, it
run's on the latest version of Ubuntu, and it has a number of steps...

```yaml
jobs:
  Deploy-Production:
    runs-on: ubuntu-latest
    steps:
```


#### Steps {#steps}

<!--list-separator-->

-  Checkout and Cleanup

    We actually have to checkout two different repos. First let's checkout the
    Sylvan repo to the root of our action:

    ```yaml
          - name: Checkout Sylvan
            uses: actions/checkout@v2
            with:
              repository: pard68/sylvan
    ```

    And then we want to remove the `.git/`  from the Sylvan project, otherwise Vercel
    will link the deployment to the wrong repo (you can read more about this in the
    [blog post]({{< relref "blog/vercel-woes.md" >}}) I wrote detailing the matter):

    ```yaml
          - name: "Remove the Sylvan .git dir"
            run: "rm -rf .git"
    ```

    Now we checkout the org repo (which is the repo this action runs it, but you
    cannot view it since it's a private repo), and we check it out to the `org/`
    directory within the Sylvan project:

    ```yaml
          - name: Checkout Org content
            uses: actions/checkout@v2
            with:
              path: org/
    ```

    Now we're going to remove any files that I have marked as private:

    ```yaml
          - name: "Remove files marked :PRIVATE: t"
            run: 'find ./org -type f -exec grep -q "^:PRIVATE: t\$" {} \; -delete'
    ```

    Vercel needs a `.git` directory, so we will copy the directory from the org
    project into the root of the action, this way the deployments will be marked
    with the right commits:

    ```yaml
          - name: Move the org repo .git dir to root to satisfy Vercel
            run: mv org/.git ./
    ```

    And we remove the `remote` because Vercel keeps trying to link directly to this
    repo and that breaks stuff:

    ```yaml
          - name: Expunge git remote
            run: git remote remove origin
    ```

<!--list-separator-->

-  Getting the right files to the right places

    Next on the list of steps, we want to move everything in the org directory (and
    thus repo) into the `public` directory of the Sylvan project. We do this because
    any text files, or pictures will now be accessible by the server. We include the
    `.attach` directory because this is where attachments are, and I like to ues
    attachments for pictures, especially screenshots and pictures I take on my
    phone:

    ```yaml
          - name: Merge org content with default Sylvan content
            run: mv org/* public/
          - name: Move over needed, hidden directories
            run: mv org/.attach/ public/
    ```

<!--list-separator-->

-  Setting the Timezone

    I don't think this actually is doing anything, but I'm setting it just in
    case...

    ```nil
          - name: Set Timezone
            uses: szenius/set-timezone@v1.0
            with:
              timezoneLinux: "Americas/Chicago"
    ```

<!--list-separator-->

-  Build and Deploy!

    Now we install the Vercel CLI, pull in the Vercel project, build it, and push
    it. This is all right from the Vercel documents with no changes:

    ```yaml
          - name: Install Vercel CLI
            run: npm install --global vercel@latest
          - name: Pull Vercel Environment Information
            run: vercel pull --yes --environment=production --token=${{ secrets.VERCEL_TOKEN }}
          - name: Build Project Artifacts
            run: vercel build --prod --token=${{ secrets.VERCEL_TOKEN }}
          - name: Deploy Project Artifacts to Vercel
            run: vercel deploy --prebuilt --prod --token=${{ secrets.VERCEL_TOKEN }}
    ```
