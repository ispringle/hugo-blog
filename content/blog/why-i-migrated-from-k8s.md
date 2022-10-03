+++
title = "Why I Migrated From K8s"
author = ["Ian S. Pringle"]
tags = ["infra"]
draft = false
+++

At the time of this writing, my blog is still hosted with Vercel, however I
think I will eventually move it to my VM. However, all the other sites and
services I host have been migrated to my new VM (at the time of writing, only
Nextcloud hasn't been migrated, but it never made it into Kubernetes anyway).
Why the switch? Well, I started using kubernetes after I took a job as a
consultant doing SRE work. I wanted to have my personal infrastructure mirror
the work I was doing professionally. Now that I am not doing that kind of work
anymore, there is less motivation to keep a K8s cluster going. Additionally and
primarily, I had a very hard time figuring out how to migrate Nextcloud to
Kubernetes due to the amount of data our family Nextcloud instance already had
and decisions I had made about the Nextcloud setup initially. So I opted to keep
it on a VM and migrate everything else to K8s. Well, that costs a lot of money,
I decided to cut costs and that meant either looking into moving Nextcloud to
K8s or moving K8s back to a VM.

The decision was made for me when I went to
update my resume and noticed that fluxcd was no longer automatically updating
docker images when new ones were available. I looked into the problem for an
hour or so but eventually just decided this was the writing on the wall and it
was time to move back to a VM. I did the migration in a morning. It took about
two hours, most of that two hours was spent trying to get my
ansible+docker-compose stuff working. Technically that all worked just fine, but
I could not get the dockerized Traefik proxy server to work. When I looked into
alternatives I found that Caddy had matured a lot since I was last shopping for
a reverse proxy. I tried the dockerized Caddy and it didn't work, and then upon
reflection I decided it might be a good idea to run my server on the OS level
and then proxy to the container with ports. You can checkout the [server setup]({{< relref "#d41d8c" >}}) in
my literate config file.
