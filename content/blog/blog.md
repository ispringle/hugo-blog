+++
title = "Blog"
author = ["Ian S. Pringle"]
draft = false
+++

## General Posts <span class="tag"><span class="general">general</span></span> {#general-posts}


### <span class="org-todo done DONE">DONE</span> Perhaps A New Path For Sylvan {#perhaps-a-new-path-for-sylvan}

To me the idea of "sylvan" is a tool meant to easily create beautiful sites with
org-mode, while remaining unintrusive. For that reason, I am exploring some
alternative, non-NextJS options for Sylvan. There's a few reasons for this:

1.  As I've expressed before, I'm not 100% thrilled with Sylvan as it exists
    today
2.  As I've expressed before, I'd love for a more emacs-centric tooling, but I
    dislike the constraints of `ox-html`
3.  I am taking a new job on October 10th and my primary frontend framework will
    be lit.dev, and I think there could be a far more interesting future for
    Sylvan in the world of web-components
4.  NextJS can produce static sites, but it's very difficult to coax it into that
    box, and I'd prefer a site that's statically compiled

So to this end, I am experimenting with new options for the _idea_ of Sylvan. My
current experiments involve augmenting `ox-html` by deriving a new export backend
for org-mode from it. The current working theory is that any element can be
templated, if desired, and if it is templated than Sylvan will use that template
file as its definition for that org element. Any elements not defined will
fallback to the `ox-html` element. I'm not sure if this is even possible, but I'm
going to give it a shot and see what I can do!


### <span class="org-todo done DONE">DONE</span> Baby, Incoming? {#baby-incoming}

We may or may not have a new baby in the next few days. My wife has been
laboring since yesterday. The midwife and I think it won't be much longer now.
What makes this awkward is I just accepted a new job and while the new job is
going to throw paternity time at me from day one, I have to get to day one
first, which isn't until October 3rd.

Might make for an interesting next three weeks.


### <span class="org-todo done DONE">DONE</span> Hard At Work {#hard-at-work}

You might not know it since we're going on almost a week since a _content_ update
to the site, but I have been doing some refactoring work on the sylvan project.
I've cut build times in half by updating the build-api. An added benefit of this
rebuild is that I can now properly use rehype plugins (retext too!). Currently
I've got rehype-raw working which means I can add `begin-export html` blocks to my
org files and they will populate on the site as actual HTML (and I think react,
but I have not tried this yet so I do not know for sure). I've also been working
on a refactor of the styling, not the actual look but the code behind it. Some
stuff will be changing but it's because of the updates to the code and then me
making minor tweaks to the looks at the same time.

All this is being done in an effort to make future features easier to implement,
such as making Sylvan configurable and useable by others.


### <span class="org-todo done DONE">DONE</span> Nice New Look {#nice-new-look}

You'll notice that my site has some nice new styles going on. I was playing with
a fork of Sylvan that used Chakra-UI, however I have since trashed that because
I couldn't get the layout I wanted (after giving up on the route I then updated
how my site is laid out and this ended up meaning that Chakra or Mantine or
really any other components library would have actually worked...). But that
sparked some renewed effort into fixing the styles. Initially I was just looking
for better heading font sizes and I found a way to do that, so then I started
playing with some other ideas, inspired by various sites I like the look of. I
think I will mostly stay with this current style, however I think I will also
implement some sort of "theme" engine eventually that will let me have loads of
themes, and each theme will then have a light and dark mode.

The current technical setup is okay -- and you can checkout how I'm doing it on
the Sylvan project repo -- but I'm not sure I am ultimately satisfied with just
having a light and dark mode. But at the same time... I really like this look
and don't want to ultimately trash it. I think my next effort will be to
differentiate between "themes" and "color scheme" so that I can then easily just
load in a different style. This will probably also involve me refactoring the
CSS into modules so as to avoid collisions and also will probably mean an easier
time interacting with the CSS via JS.

This current theme is giving me Pokemon vibes. I might try to implement my own
`border-image` with the little balls in the corners like in the original Pokemon
games. I've also been adding little easter eggs into the site just for fun,
might be worth keeping an eye out to see if you can find them all (gotta catch
'em all)!

And finally, I discovered through this that something is very wrong with either
NextJS, Vercel, or my GitHub action. Sylvan was building perfectly fine on my
machine but deploying to GH resulted in an error. Somewhere, something does not
like my usage of the `pages/api/` stuff. I have consigned to giving up on Vercel
and will eventually be moving my project to my own infrastructure. I have the
Dockerimage built and will do a little testing to make sure it's all good and
working, and then I'll flp the switch and say goodbye to Vercel. Likely this
will mean I'm moving my org repo, and perhaps even Sylvan to Gitlab because
honestly I hate GitHub Actions and have only ever experienced pain when trying
to build and deploy containers via GitHub Actions Likely this will mean I'm
moving my org repo, and perhaps even Sylvan to Gitlab because honestly I hate
GitHub Actions and have only ever experienced pain when trying to build and
deploy containers via GitHub Actions.


### <span class="org-todo done DONE">DONE</span> A Better Template Func {#a-better-template-func}

66bcba05-c427-4513-9bd4-7459ece7f5c9

In [a previous post](#my-dirt-hack-to-template-files) I talked about a function I wrote that allowed me to stub out
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


### <span class="org-todo done DONE">DONE</span> Emacs From Scratch, Part 0 {#emacs-from-scratch-part-0}

My experience with Emacs can be split into two parts:

1.  Emacs with doom
2.  Realizing Doom didn't load and quitting Emacs as quickly as possible and
    trying to figure out how to get back into Doom

I have been experiencing some annoyances with emacs lately. It hangs or lags or
chokes on large files (over 1000 lines) and it crashes sometimes. Partly out of
a desire to strip back and get a leaner emacs, and partly out of a desire to
really "try" emacs without the help of someone else's config, I am going to
attempt to build my own emacs from scratch. Of course it'll be _comfy_ in honor of
`comfy.vim`, `comfy.nvim`, and `comfy.tmux`. I'll keep you posted, likely you'll start
seeing something pop up in the </literate> index before you hear from me about it.


### <span class="org-todo done DONE">DONE</span> Maybe Easy Is Better {#maybe-easy-is-better}

I have two major shortcomings that I am working to resolve with orgmode's
default org-publish:

1.  attachments do not work out of the gate
2.  it doesn't get me closer to the ox-huge workflow of one `blog.org` file

I have looked at some other potential options for my blog. NextJS is fine, I
like it even, but I worked on refactoring to Chakra-UI and it's annoying that it
still isn't really working right. The problem currently is that this other
package I need called `Prose` has it's own default theme settings and so
everything is wonky. I guess I can get that working, but... I don't know. I
looked at the orgmode default export and it's not bad. With a little CSS love
it's actually pretty good looking! There are some hacks I could use to make it
better. Some people are doing some pretty cool and crazy stuff with their
org-publish.


### <span class="org-todo done DONE">DONE</span> Why I Migrated From K8s {#why-i-migrated-from-k8s}

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
and then proxy to the container with ports. You can checkout the [server setup]({{< relref "server.md" >}}) in
my literate config file.


### <span class="org-todo done DONE">DONE</span> Setting Up A Firewall {#setting-up-a-firewall}


#### Introduction {#introduction}

I am migrating from Kubernetes to a VM. The tl;dr on the VM is that it's an
Ubuntu machine, serving my various sites and services with Caddy. The sites and
services are all running in docker containers and the Caddy server proxies
traffic to the specific containers. I am migrating from Kubernetes because it is
expensive to run a cluster and it's far more than I need. I had migrated to
Kubernetes when I was doing consulting work as an SRE however, I am now very
focused on being a developer, and while I don't wish to lose my SRE skillset, I
also cannot justify the cost, maintenance, or cognitive load required to keep
that cluster up and working, thus a VM. The setup is pretty straightforward, it
will not be winning any awards for creativity and it's less than the pinnacle of
GitOps. You can checkout the literate server setup [here]({{< relref "server.md" >}}).

I am interested in a firewall that will expose traffic on the ports I want open
and drop all other traffic. There are a lot of tools to do this. There are
fire-and-forget tools that will do all the work for you -- these scare me, so I
didn't even consider them or look into them at all. There are simpler
configuration tools like [ipcop](http://www.ipcop.org) and Ubuntu's [UFW](https://help.ubuntu.com/community/UFW). I not only considered ufw I
started getting it setup and then hit a roadblock because I couldn't find an
easy way to `DROP` everything, including ICMP. I looked into it more and UFW can
do this but it has to be written into a before rule and at that point I might as
well see what other tools there are because I've never used UFW beyond the
command itself, and my desires seemed to require learning and writing a UFW
configuration file.

A step up from these tools are [Shorewall](https://shorewall.org/) and [ferm](http://ferm.foo-projects.org). I vaguely recall using
Shorewall before, but their website scared me off this time around. I looked at
ferm and it seemed very simple. Basically both of these tools take in their own
configuration file and then output iptables rules. I could just write the
iptables rules my self, but then again I could write the netfilter rules too! I
settled on ferm because they had clear examples and the configuration language
looks very similar to iptables rules and at the end of the day, I can export my
configuration to iptables and so if I decide to drop ferm I have not really lost
anything.


#### ferm {#ferm}

First thing's first, I checked out the example configs that come with the
default Ubuntu install of `ferm` and then cat'd together the ones I was interested
in ~~ripping off~~ using. The three I thought looked most useful are
`webserver.ferm`, `antiddos.ferm`, and `ipv6.ferm`.

<!--list-separator-->

-  Examples

    <!--list-separator-->

    -  webserver.ferm

        <a id="code-snippet---usr-share-doc-ferm-examples-webserver.ferm"></a>
        ```shell
        # -*- shell-script -*-
        #
        # Ferm example script
        #
        # Firewall configuration for a web and SMTP server.
        #
        # Author: Max Kellermann <max@duempel.org>
        #

        @def $NET_TRUSTED = 195.135.144.144/28;

        table filter {
            chain INPUT {
                policy DROP;

                # connection tracking
                mod state state INVALID DROP;
                mod state state (ESTABLISHED RELATED) ACCEPT;

                # allow local connections
                interface lo ACCEPT;

                # respond to ping
                proto icmp icmp-type echo-request ACCEPT;

                # remote administration from the company network
                saddr $NET_TRUSTED proto tcp dport ssh ACCEPT;

                # our services to the world
                proto tcp dport (http https smtp) ACCEPT;

                # the rest is dropped by the above policy
            }

            # outgoing connections are not limited
            chain OUTPUT policy ACCEPT;

            # this is not a router
            chain FORWARD policy DROP;
        }
        ```

    <!--list-separator-->

    -  antiddos.ferm

        <a id="code-snippet---usr-share-doc-ferm-examples-antiddos.ferm"></a>
        ```shell
        # -*- shell-script -*-
        #
        # Ferm example script
        #
        # Firewall configuration to prevent basic tcp DoS/DDoS attacks
        #
        # Authors: Vlad Glagolev <enqlave@gmail.com>, Stepan Rogov <rogov_sa@mail.ru>
        #

        @def &ANTIDDOS($ports, $seconds, $hits, $time, $exceptions) = {
                proto tcp dport $ports @subchain "ddos_check" {
                        # allow every exception as-is
                        saddr $exceptions ACCEPT;

                        # connection tracking
                        mod conntrack ctstate (ESTABLISHED RELATED) ACCEPT;

                        # check for IPs overloading $hits/$seconds rate and block them
                        mod recent name "ddos_check" rcheck seconds $seconds hitcount $hits @subchain "ddos" {
                                mod recent set name "ddos" NOP;

                                DROP;
                        }

                        # register a packet in "ddos_check" list
                        mod recent set name "ddos_check" NOP;

                        # check IP in "ddos" list
                        # if it exists and had been registered in the last $time seconds -- drop it
                        mod recent name "ddos" rcheck seconds $time DROP;

                        # remove packet from "ddos" list
                        mod recent name "ddos" remove NOP;

                        # allow ONLY new connections
                        mod conntrack ctstate NEW ACCEPT;

                        DROP;
                }
        }

        table filter {
                chain INPUT {
                        policy DROP;

                        # connection tracking
                        mod state state INVALID REJECT;
                        mod state state (ESTABLISHED RELATED) ACCEPT;

                        # allow local connections
                        interface lo ACCEPT;

                        # ban ip addresses for 1 day which connect more than 50 times in 3 seconds,
                        # exception is IP: 94.29.90.101
                        &ANTIDDOS((80, 443), 50, 3, 86400, 94.29.90.101);

                        # the rest is dropped by the above policy
                }

                # outgoing connections are not limited
                chain OUTPUT policy ACCEPT;

                # this is not a router
                chain FORWARD policy DROP;
        }
        ```

    <!--list-separator-->

    -  ipv6.ferm

        <a id="code-snippet---usr-share-doc-ferm-examples-ipv6.ferm"></a>
        ```shell
        # -*- shell-script -*-
        #
        # Ferm example script
        #
        # IPv6 demo.
        #
        # Author: Max Kellermann <max@duempel.org>
        #

        domain ip6 table filter {
            chain INPUT {
                policy DROP;

                # connection tracking
                mod state state INVALID DROP;
                mod state state (ESTABLISHED RELATED) ACCEPT;

                # allow local connections
                interface lo ACCEPT;

                # allow ICMP (for neighbor solicitation, like ARP for IPv4)
                proto ipv6-icmp ACCEPT;

                # allow SSH connections
                proto tcp dport ssh ACCEPT;

                # ident connections are also allowed
                proto tcp dport auth ACCEPT;

                # the rest is dropped by the above policy
            }

            # outgoing connections are not limited
            chain OUTPUT policy ACCEPT;

            # this is not a router
            chain FORWARD policy DROP;
        }

        ```

<!--list-separator-->

-  The Configuration

    ferm's configuration is pretty straightforward, you tables and chains, you can
    specific a domain (ip, ipv6) for the table, and you can target specific
    predefined policies (ACCEPT, DROP, etc.). I am not a networking guy or a
    firewall guru, but I think I have enough understanding to write something...

    <!--list-separator-->

    -  The filter Table

        We'll write to the default table `filter`:

        ```shell
        table filter {
        ```

        And we'll start with the `INPUT` chain:

        ```shell
            chain INPUT {
        ```

        Seems like you start with a your catch-all, in this case I want to `DROP` anything
        that isn't explicitly allowed, hopefully this will prevent some attacks just by
        not advertising this server exists.

        ```shell
                policy DROP;
        ```

        I am still not entirely sure what "connection tracking" is or means, but this
        rules controls this and here is a [brief synopsis](https://web.mit.edu/rhel-doc/4/RH-DOCS/rhel-sg-en-4/s1-firewall-state.html) of the idea.

        ```shell
                mod state state INVALID DROP;
                mod state state (ESTABLISHED RELATED) ACCEPT;
        ```

        This is rather self-explanatory, we allow local connections through the
        firewall, it probably is safe to say this won't hurt, if an attacker is coming
        from local we got bigger fish to fry...

        ```nil
                interface lo ACCEPT;
        ```

        Contrary to the default settings, I do _not_ want to respond to ICMP:

        ```nil
                proto icmp icmp-type echo-request DROP;
        ```

        Now for the things that this server needs to have open. The use of `dport` refers
        to the intended destination of the request.

        ```nil
                proto tcp dport (http https) ACCEPT;
        ```

        We also want to accept SSH connections. I might change this in the future to be
        over a port other than the default, but for now 22 is good enough!

        ```shell
                proto tcp dport ssh ACCEPT;
        ```

        Everything else is dropped. We also want to allow all outgoing connections,
        though I might look into changing this eventually and would like to log all
        outgoing connections at the very least. And since we're a web server and are not
        in the business of routing for other machines, we will drop all `FOWARD` requests.

        ```shell
            }
            chain OUTPUT policy ACCEPT;
            chain FORWARD policy DROP;
        }
        ```

    <!--list-separator-->

    -  Anti-DDoS logic

        To mitigate DDoS attacks, we can define a function to track requests over time
        for a specfic IP address and if it exceeds a given threshold, we block that
        address for some length of time. This comes right from the examples document,
        except I removed the IP address exceptions logic because I don't have a static
        address and the address I do get is sometimes shared with other Starlink
        customers.

        ```shell
        @def &ANTIDDOS($ports, $seconds, $hits, $time) = {
                proto tcp dport $ports @subchain "ddos_check" {
                        mod conntrack ctstate (ESTABLISHED RELATED) ACCEPT;
                        mod recent name "ddos_check" rcheck seconds $seconds hitcount $hits @subchain "ddos" {
                                mod recent set name "ddos" NOP;
                                DROP;
                        }
                        mod recent set name "ddos_check" NOP;
                        mod recent name "ddos" rcheck seconds $time DROP;
                        mod recent name "ddos" remove NOP;
                        mod conntrack ctstate NEW ACCEPT;
                        DROP;
                }
        }
        ```

        To use this we want to do two things, first we need to define this _before_ our
        filter table declaration and in that table we want to replace the line(s) that
        declare ports we accept with the following which says "check requests to ports
        22, 88, and 443 to see if the requester has made more than 50 requests in the
        last three seconds, and if so, drop their request and ban them for 86400
        seconds." The docs say to do `(22, 80, 443)` however if you try to do that `ferm`
        will give a warning and rejects the configuration file. According to the error,
        arrays should not be comma separated but should just have spaces. I have opened
        an [issue](https://github.com/MaxKellermann/ferm/issues/93) for this and hopefully it'll be resolved before anyone else gets
        confused.

        ```shell
                &ANTIDDOS((22 80 443), 50, 3, 86400);
        ```

    <!--list-separator-->

    -  IPv6

        My ipv6 filter table is also right from the docs, except I am dropping ICMP and
        I'm routing traffic through the anti-DDoS logic.

        ```shell
        domain ip6 table filter {
            chain INPUT {
                policy DROP;
                mod state state INVALID DROP;
                mod state state (ESTABLISHED RELATED) ACCEPT;

                interface lo ACCEPT;
                proto ipv6-icmp DROP;

                &ANTIDDOS((22 80 443), 50, 3, 86400);
            }
            chain OUTPUT policy ACCEPT;
            chain FORWARD policy DROP;
        }
        ```

    <!--list-separator-->

    -  Putting It Together

        All of that boils down to this fairly succinct ruleset that can be put into
        `/etc/ferm/ferm.conf` and then just restart the service. You might actually want
        to call this the first time with `ferm --interactive` which will kill ferm if you
        get locked out due to firewall rules.

        ```shell
            @def &ANTIDDOS($ports, $seconds, $hits, $time) = {
                proto tcp dport $ports @subchain "ddos_check" {
                    mod conntrack ctstate (ESTABLISHED RELATED) ACCEPT;
                    mod recent name "ddos_check" rcheck seconds $seconds hitcount $hits @subchain "ddos" {
                        mod recent set name "ddos" NOP;
                        DROP;
                    }
                    mod recent set name "ddos_check" NOP;
                    mod recent name "ddos" rcheck seconds $time DROP;
                    mod recent name "ddos" remove NOP;
                    mod conntrack ctstate NEW ACCEPT;
                    DROP;
                }
            }
            table filter {
                chain INPUT {
                    policy DROP;
                    mod state state INVALID DROP;
                    mod state state (ESTABLISHED RELATED) ACCEPT;
                    interface lo ACCEPT;
                    proto icmp icmp-type echo-request DROP;
                    &ANTIDDOS((22 80 443), 50, 3, 86400);
                }
                chain OUTPUT policy ACCEPT;
                chain FORWARD policy DROP;
            }
            domain ip6 table filter {
                chain INPUT {
                    policy DROP;
                    mod state state INVALID DROP;
                    mod state state (ESTABLISHED RELATED) ACCEPT;

                    interface lo ACCEPT;
                    proto ipv6-icmp DROP;

                    &ANTIDDOS((22 80 443), 50, 3, 86400);
                }
                chain OUTPUT policy ACCEPT;
                chain FORWARD policy DROP;
            }
        ```


#### Conclusion {#conclusion}

I will likely be playing with this for a while and change things to adapt with
time. I would like to get better observability into what is being hit, by whom,
etc. and when I have that I can review incoming requests, pick out problematic
patterns and work to shut them down. This will mean changes to the firewall. My
ever-up-to-date firewall configuration can be found in my [literate server
configuration]({{< relref "server.md" >}}) under the firewall heading. In the future, I would like to also
add fail2ban to my setup, however I have [read](https://tina.pm/blog/posts/Setting_up_my_server:_netfilter/#comments) something about how ferm can wipe
out f2b's tables and I want to research into this some more before I incorporate
it into my setup. Another idea I had was to use ferm to create the iptables
values and then directly insert them into iptables. This might or might not be a
good idea, but it's something I might look into.


### <span class="org-todo done DONE">DONE</span> What's Next For Sylvan {#whats-next-for-sylvan}

For those who do not know [Sylvan](https://github.com/pard68/sylvan) is my NextJS app that org files and creates
webpages. It's not a static-site generator technically since it's doing
server-side rendering, but I currently do not have it setup so that the org
files are updated on the fly either (ie content only gets updated when a
deployment runs). Fixing this is on the roadmap, but first I want to get it
setup to be configurable so that I can, for example, change the header, the
favicon, the overall title, etc. from my org repo. This will do two things,
first and foremost it will mean Sylvan can be used by others without having to
fork the repo, and second it will mean I can make changes to my website without
changing the framework that manages the content. I haven't exactly figured out
how I plan to do this just yet. I think I will end up going with a `sylvan.org`
file that will have some K/Vs in a drawer and Sylvan can consume that at startup
via the Uniorg module and then from there the `<nav>` links and other things can
be updated.


### <span class="org-todo done DONE">DONE</span> Still Undecided {#still-undecided}

Me, the jury of one, is still out on whether I will continue with the
Uniorg+NextJS or attempt to find a different solution. The current trouble?
Parsing of Orgmode verse blocks results in a `<pre>` , which is not at all what I
want. I could make most of the verse blocks, most of the time, look good. But
what if it contains superscript text? In Org that looks like a `^{I am
superscript}` or a `^superscript`. But when that translates to HTML it ends up
being wrapped as `<sup>I am superscript</sup>`  which doesn't work with the `<pre>`
block since these blocks render text verbatim. This is a problem for Bible
verses which have whitespace formatting (which is that the Orgmode verse block
is meant to preserve) but also have superscript text for the verse numbers. Take
a look at Isaiah 8:13:

<div class="verse">

﻿<sup>13</sup> It is Yahweh of hosts whom you should regard as holy.<br />
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your fear,<br />
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your cause of trembling.<br />

</div>

This is extra tricky, not only is there some whitespace formatting, but the
superscript is the very first element in the text which, in Orgmode, requires
that you use a special zero-width, non-breaking space character.

What to do? I don't know. I was just getting happy with the idea of continuing
with NextJS+Uniorg too. Suffice to say, I am going to continue plodding away at
my [build](../build) system for org-publish, because the exported Orgmode HTML does a
wonderful job of properly displaying this exact situation:

```html
<p class="verse">
﻿<sup>13</sup> It is Yahweh of hosts whom you should regard as holy.<br>
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your fear,<br>
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your cause of trembling.<br>
﻿<sup>14</sup> Then He shall become a sanctuary;<br>
&nbsp;&nbsp;&nbsp;&nbsp;But to both the houses of Israel, a stone to strike and a rock to stumble over,<br>
&nbsp;&nbsp;&nbsp;&nbsp;And a snare and a trap for the inhabitants of Jerusalem.<br>
﻿<sup>15</sup> And many will stumble over them;<br>
&nbsp;&nbsp;&nbsp;&nbsp;Then they will fall and be broken;<br>
&nbsp;&nbsp;&nbsp;&nbsp;They will even be snared and caught.”<br>
</p>
```


### <span class="org-todo done DONE">DONE</span> Commonplacing {#commonplacing}

So, I'm thinking about the concept of digital gardens, the ideas behind
Zettelkasten, and my own interpretation of that I am _currently_ calling "[Grok]({{< relref "loci/grok.md" >}})". I
was reading on another _digital garden_ about this historic concept called
"[commonplace books](https://en.wikipedia.org/wiki/Commonplace_book)" or "commonplaces". The idea goes back further to this Latin
term called _locus communis_, and it goes back even further to the Greek idea of
"τόπος κοινός" (topos koinos) or "[literary topos](https://en.wikipedia.org/wiki/Literary_topos)". John Locke even wrote a book
on this idea called _[A New Method of Making Common-Place-Books](https://archive.org/details/gu_newmethodmaki00lock)_. I'm interested in
this for a couple reasons; first it's interesting to see that my desire to have
_some place_ to store my ideas is shared with many in antiquity, and second as I
am refining this concept and building on it and how I want to treat it
conceptually as well as in practice, it's handy to see how others, both present
and past, have treated this same sort of concept. I think I ultimately dislike
the Zettelkasten, at least for _me_ because it is too fine-grained and too keyed
into specfic topics. I am not a researcher, I just like a broad array of things
and I enjoy seeing their interconnectedness.

This commonplacing (not sure if that's a coined term or not) idea is something I
find really attractive because instead of being a repository for specfic, well
crafted, and atomic thoughts, it is instead a repository for a myriad of
thoughts, quotes, ideas, and excerpts on a whole array of topics.

Likely, at some future time I will update the name of my digital garden to be
more in tune with this historic concept. We'll see...


### <span class="org-todo done DONE">DONE</span> Publishing with org-publish {#publishing-with-org-publish}

While looking further into ox-hugo I decided to also look further into just
bog-standard org-publish. I already had some elisp to build out my org files to
html and I decided to break that elisp down into a literate program that is then
compiled into a build directory which contains all the shell, elisp, CSS, and
Dockerfiles I need to build and deploy a website built with org-publish. I am
not _currently_ using this to deploy this or any other website, but I was pretty
happy with the result and wanted to share. I think I could quickly get a website
that is almost identical to my current NextJS site.

You can check out [the build script]({{< relref "build" >}}) and see how I'm doing it.


### <span class="org-todo done DONE">DONE</span> New Puppy! {#new-puppy}

I got a new puppy today! I still don't have a name for him. He's a Catahoula
Leopard dog. That tail is gonna take some getting used to, it's not docked,
that's just what they call a "bob" tail, it's natural and common on this breed.

{{< figure src="/ox-hugo/_20220823_162424image (1).png" >}}


### <span class="org-todo done DONE">DONE</span> Playing With Hugo {#playing-with-hugo}

So I've spent some time playing with Hugo and more specifically, I've been
playing with `ox-hugo` which is a way of exporting Org files to Markdown that
plays nicely with Hugo. I'm not happy with my current blog setup. I like the
Sylvan site and NextJS, but blogging with it is cumbersome, in part because of
the Org parser I am using. In JavaScript land there are two Org parsers, one is
mostly abondonware and the other is called Uniorg. It's great for what it is,
but it's not refined, it has a very poor way of interacting with it, it feels
fragile, and the author has expressed disinterest in support rather common and
crucial Orgmode features -- such as multiple drawers in a document,
non-PROPERTIES drawers (ie LOGBOOK), and subtrees.

In my opinion none of these are inherintly a deal breaker, and all are things I
could remedy by forking the project and building my own off of it. But, I'm not
interested in spending a huge time on this and if I did, why limit myself to
JavaScript? I see a few potential options:

1.  Use Hugo, it works out of the box and produces a very satisfactory website
    with nearly no fuss
2.  Fork Uniorg and add the myriad features I'd like to see
3.  Use another parser all together, such as [this python parser](https://orgparse.readthedocs.io/en/latest/), and then decide
    what to do about serving

Some things in ox-huge/hugo's favor:

-   it just works
-   converting my stylings to hugo shouldn't be too hard
-   ox-hugo supports a LOT of org features
-   files attached to org files are ported
-   it's static

Some things that I'm a little concerned about:

-   No server means less cool things
-   No node means harder to do cool JS things

I might flip the switch and if I do, you'll know...


### <span class="org-todo done DONE">DONE</span> The Time Is Wrong! {#the-time-is-wrong}

Well... it has happened again. The last time it was because of the YAML format
interpretting timestamps as UTC unless explicitly set otherwise. And I [fixed that
issue]({{< relref "the-case-of-the-wrong-date" >}})... but that won't help this, since I gave up on using Contentlayer like
two days later 😭.

I'm not sure the reason for the problem this time. I even [updated](https://github.com/szenius/set-timezone) the timezone
for the GH action so that it would be set to CST. Nonetheless, the date entries
in the indices (anywhere that is listing blog posts or content) is a whole day
_earlier_ than the actual timestamp for the entry.


### <span class="org-todo done DONE">DONE</span> Vercel Woes {#vercel-woes}

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


### <span class="org-todo done DONE">DONE</span> Literate Dots Part 1 ZSH {#literate-dots-part-1}

I had mentioned [tangling all the things](#tangle-all-the-things) yesterday, and I've gone ahead and
gotten a literate Zsh config up and running this morning. I've also created a
new repo for it [here](https://github.com/pard68/literate-dotfiles).

Setup-wise, I went through a few different configurations to keep this DRY and
eventually settled on setting global tangle settings in the properties drawer
and then specifying each block's output file in the source block. I might update
this eventually, but I couldn't convince babel to run any other way. The two
things needed to get this working are the following in the file's property
drawer:

```org
:PROPERTIES:
:header-args: :tangle yes :comment link :mkdirp yes :padline no :noweb tangle
:END:
```

And then each source block must specify the output file like such:

```org
:tangle ~/.zshenv
```

After that it's merely a matter of writing your dotfiles out inside the source
blocks, organizing them in the orgfile as you like, and providing any comments
you care to provide. As an example, I am going to just copy/paste the bulk of my
zsh config from the literate-dotfiles repo into the rest of this post.

<!--list-separator-->

-  Environment Variables

    Sadly, ZSH sorta sucks and the `.zshenv` has to live in the home directory, so
    we're going to set that up and tell it to look in `.config/zsh/` for all the
    rest of the zsh config files we might use. So we'll setup the zshenv file to
    have all the right XDG settings, plus point all the various other things that
    need to be told to use .config to do so.

    <!--list-separator-->

    -  XDG and Zsh paths

        ```sh
        export XDG_CONFIG_HOME="$HOME/.config"
        export XDG_DATA_HOME="$HOME/.local/share"
        export XDG_DATA_HOME="$HOME/.cache/"

        export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
        export HISTFILE="$ZDOTDIR/zhistory"
        ```

    <!--list-separator-->

    -  HIST

        We want to make sure our history is nice and long. It's 2022, so we shouldn't
        worry too much about disk space or RAM... I hope

        ```sh
        export HISTSIZE=10000
        export SAVEHIST=10000
        ```

    <!--list-separator-->

    -  EDITOR

        Let's set our editor quickly so we can make sure we're never far away from
        emacs.

        ```sh
        export EDITOR="/usr/local/bin/emacsclient"
        export VISUAL="/usr/local/bin/emacsclient"
        ```

    <!--list-separator-->

    -  $PATH

        We'll setup some `$PATH` stuff now too. I really hate editing one-liner `$PATH`
        exports, so we'll just do one per line, why not? Maybe there is a cool way to
        use org and iterate over a list of path values and concatenate them together,
        but IDK how to do that right now...

        ```sh
        export PATH="$HOME/.cargo/bin:$PATH"
        export PATH="$HOME/go/bin:$PATH"
        export PATH="$HOME/.local/bin:$PATH"
        export PATH="/usr/local/bin:$PATH"
        export PATH="/usr/bin:$PATH"
        export PATH="/bin:$PATH"
        export PATH="/usr/local/sbin:$PATH"
        export PATH="/usr/local/go/bin/:$PATH"
        export PATH="$HOME/.emacs.d/bin/:$PATH"
        export PATH="$HOME/.npm-global/bin:$PATH"
        ```

<!--list-separator-->

-  Zshrc

    <!--list-separator-->

    -  A E S T H E T I C

        Your terminal, in Technicolor!

        ```sh
        autoload -U colors && colors
        PROMPT="%B%F{magenta}λ%f%b "
        RPROMPT="%*"
        ```

    <!--list-separator-->

    -  Zsh Options

        [Here](https://zsh.sourceforge.io/Doc/Release/Options.html) is a list of all the zsh options that can be set.

        ```sh
        setopt HIST_SAVE_NO_DUPS
        setopt INC_APPEND_HISTORY
        setopt HIST_IGNORE_SPACE

        setopt AUTO_CD
        setopt AUTO_PUSHD
        setopt PUSHD_IGNORE_DUPS
        setopt PUSHD_SILENT

        REPORTTIME=3
        ```

    <!--list-separator-->

    -  Ghetto Jump

        There are some neat "jump" plugins like `j` and `z`. But we're just going to DIWhy it!

        ```sh
        alias d='dirs -v'
        for index ({1..9}) alias "$index"="cd + ${index}"; unset index
        ```

    <!--list-separator-->

    -  Completion

        ```sh
        autoload -U compinit
        zstyle ':completion:*' menu select completer _complete _correct _approximate
        zmodload zsh/complist
        compinit
        _comp_options+=(globdots)
        ```

<!--list-separator-->

-  Aliases

    Before we make an alias file, let's source them from the zshrc file.

    ```sh
    source $ZDOTDIR/aliases
    ```

    Okay, now for some aliases.

    ```sh
    alias c!=clear
    alias g=git
    alias ga="git add"
    alias ga.="git add ."
    alias gb="git branch"
    alias gbd="git branch -D"
    alias gc="git commit"
    alias gcm="git commit -m"
    alias gca="git commit --amend"
    alias gcm!!="git add .; git commit -m "Update!"; git push"
    alias gcl="git clone"
    alias gco="git checkout"
    alias gd="git diff"
    alias gl="git log"
    alias gm="git merge"
    alias gpl="git pull"
    alias gps="git push"
    alias gps!="git push --force"
    alias gpsu="git push -u origin master"
    alias gri="git rebase -i"
    alias gs="git status"

    alias l="ls"
    alias la="ls -a"
    alias ll="ls -l"
    alias lla="ls -la"
    ```


### <span class="org-todo done DONE">DONE</span> Indie Web {#indie-web}

The [Indie Web](https://indieweb.org) is basically an attempt at providing standards and tooling to
create websites that are federated and connected to one another. Some of the
things they have include _IndieAuth_, _Webmentions_, _h-cards_, _h-entries_, and
syndication with _POSSE_. Almost all of these things are just standards that you
can implement on your site, like the _h-\*_ stuff, and allow for other sites and
tools to interact with your own website. Some neat things that this allows for
are mostly automatic "webmentions", which are anything from comments on a post,
to response posts, and "shares". An _h-card_ is basically a business card that
other sites can find in your `<head>`.

Anywho, I had this implemented before on my 0x44.pw website and I am going to
try to really implement the entire catalog of IndieWeb features here on _this_
site in Sylvan. Why? I think it's a cool exercise and allows for some neat
interactions.


### <span class="org-todo done DONE">DONE</span> Tangle All The Things {#tangle-all-the-things}

So I had this idea last night, and like with all great ideas I googled it see if
anyone else had come up with it first. To my dismay, I was far from the first
person to have this awesome idea. The idea? Literate dotfiles!

Imagine how great it'd be! You write a single file, let's call it `dotfiles.org`
and then inside of that you write all your dotfiles and document them. Then you
just use org's tangle feature to pull all the config code and stick it into all
the right files in all the right directories. I think I will try this first with
a ZSH config, since I'm getting really annoyed with Fish in emacs and Fish
inside of vterm inside of emacs seems sort of overkill.


### <span class="org-todo done DONE">DONE</span> My Dirty Hack to Template Files {#my-dirt-hack-to-template-files}

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


### <span class="org-todo done DONE">DONE</span> Keeping Some Content Private A Stop-Gap {#keeping-some-content-private}

So I'd like to keep _some_ content in my org repo private -- things like todos,
journals, and unfinished posts. Ultimately I'd like to put this sort of stuff
behind a login, but as a stop-gap I'm thinking to go the brute-force way and
`rm` it. My stop-gap is to delete any file containing the line `:PRIVATE: t`.
I'm going with the property route so that in the future I can update my solution
to be a bit more elegant without having to do any additional work to alter my
files.

So to make this work all I have to do is add the following to a step in my GitHub deployment action:

```shell
find . -type f -exec grep -q "^:PRIVATE: t\$" {} \; -delete
```

With this in place it is only a matter of annotating the files I don't want to
publish with the new property and they should cease to be deployed.

My ultimate goal is to have Uniorg change the slug for files annotated with this
property and prefix a `private` to the path or something along those lines. Then
I can setup Cloudflare to put a login prompt in front of any page that contains
this specific path. The key is going to be ensuring that whatever this path
alteration ends up being isn't something that I might accidentally use in some
other file -- say a blog post talking about private files...


### <span class="org-todo done DONE">DONE</span> Deployment. How Do? {#deployment-how-do}


#### Introduction {#introduction}

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


#### Deploying to Vercel from a GitHub action {#deploying-to-vercel-from-a-github-action}

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


#### Trigger builds when Sylvan repo changes {#trigger-builds-when-sylvan-repo-changes}

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


#### Update! {#update}

Just a quick update, I have written my Github Action as a literate file, so now
you can check it out [here]({{< relref "github-workflow.md" >}}), the nifty thing about this is that the file is what I
use to compile the yaml for the GH actions, so it is in some sense a "living"
document and will always reflect what I am actually using.


### <span class="org-todo done DONE">DONE</span> Migrating To Org Mode {#migrating-to-org-mode}

I am back to (doom)emacs+org-mode, again... again... again...

Why? I like it!

I tried to use ox-hugo. And it seemed nice, but way to much extra. Org mode can
compile org files to html, why convert the files to markdown first? Just didn't
make sense. ~~I think I shall try sticking with stock org-mode for publishing.~~

I ended up finding a wonderful package called [Uniorg](https://github.com/rasendubi/uniorg), which is a parser that
takes Org files and generates Unified ASTs that can then be manipulated by tools
like Rehype to modified and spit-out HTML. This removes the annoyance of ox-hugo
but means I am able to have a bit more control over my contnet -- or at least
_easier_ access to control -- than with org's own publishing feature. Though, I
did get the org-publish mostly working and producing acceptable HTML, I think.

Uniorg isn't the end-all-be-all, but it's fairly extensible and it is actively
maintained, unlike orgajs, which seems to have either died or at least has been
put on the back-burner by the maintainers. This does mean I have no need for
contentlayer, I have had the thought that maybe contentlayer could get org
compatibility added through the usage of Uniorg, however the more I think about
that the less sense it makes, org files are already much more structured and
defined than markdown with arbitrary frontmatter and Uniorg already is turning
the org files into data. For the mean time, I have gutted the contentlayer
aspects of [Sylvan](https://github.com/pard68/sylvan) and put Uniorg in. My next step with that project is going to
be figuring out how to build Sylvan with a remote repository for content, thus
meaning I can keep my org directory free of the Sylvan related project files and
vice versa.

A couple things I'd like to see out of Uniorg:

-   Expose drawers for gathering data from (I have managed to get `PROPERTIES`
     drawers, but none other, and the current solution for `PROPERTIES` drawers
    dumps all data from _all_ the drawers in a file into one, meaning if multiple
    headings have drawers than that data is getting combined and overwritten)
-   Respect the datatree
-   Less boilerplate

Other than that, I am fairly happy with Uniorg thus far. I will say, in
org-publish's favor, I liked the styling and layout of the default org-publish
function so much that I have endeavored to incorporate some of the asthetics
into my NextJS powered site.


### <span class="org-todo done DONE">DONE</span> The Case of the Wrong Date {#the-case-of-the-wrong-date}

I noticed a funny thing happening, the dates for my posts were wrong. The source
of the post would contain the correct value in the frontmatter, for example the
date of my [first post](_blog/hello-world) was `2022-07-25` but when that was
rendered by NextJS and spit out on the website it was coming in a whole day
/earlier_, ie `2022-07-24`. At first I thought the culprit was the date library,
`date-fns`, so I dropped into a node shell to see if that was the case...

```shell
> date.format(date.parseISO('2022-07-29'), 'LLLL d, yyyy') 'July 29, 2022'
```

So `date-fns` is not the culprit. Next step, `console.log`!

```javascript
  const postDate = format(parseISO(post.date), 'LLLL d, yyyy')
  console.log(postDate, post.date) // July 24, 2022 2022-07-25T00:00:00.000Z
```

Weird right? The input is the 25th but the output is the 24th. Let's see what
the date-fns documentation has to [say](https://date-fns.org/v2.29.1/docs/Time-Zones#overview):

> Libraries like Moment and Luxon, which provide their own date time classes,
> manage these timestamp and time zone values internally. Since date-fns always
> returns a plain JS Date, which implicitly has the current system's time zone,
> helper functions are needed for handling common time zone related use cases.

Okay, not so weird maybe! But I'm not sure the answer is going to be easy. The
documentation goes on to explain how to update a datetime stamp to either
reflect the time in a given locale based on the UTC time _or_ update a locale
based timestamp to reflect UTC time. What we need is to update the _timezone_, the
datetime is right, the timezone is wrong in this particular case.

Sadly the JavaScript `date` doesn't really have this concept and so it sounded
like it might be easier to correct the problem at the source... and I figured
that source was Contentlayer, since it was parsing my frontmatter.

I did some more dgging and it looks like someone who works on Contentlayer
actually attempted addressing this issue [already](https://github.com/contentlayerdev/contentlayer/issues/9#issuecomment-980280572) and the tl;dr is that it's
actually not something they can fix because it's being determined by something
in front of Contentlayer, and the dev suggested it might be an issue with
gray-matter, the frontmatter parser they're using.

I went to the [repo](https://github.com/jonschlinkert/gray-matter/) for gray-matter and poked around, but after looking at [how
this library works](https://github.com/jonschlinkert/gray-matter/blob/master/lib/engines.js#L16), this is not the culprit either.

Next step was to check in on `js-yaml`, which is what gray-matter is using to
parse the frontmatter -- at least in my case since I'm using yaml frontmatter.

The story there is likely that I need to change how I present the date, as it
wants either the date format I'm using (ie `2022-07-29`) or it wants a full blown
timestamp. If you use the full blown timestamp than it can set the timzezone,
but otherwise you're stuck with UTC it looks like. And this isn't just that
`js-yaml` library, but in fact this is a part of the [YAML standard](https://yaml.org/type/timestamp.html):

> If the time zone is omitted, the timestamp is assumed to be specified in UTC.
> The time part may be omitted altogether, resulting in a date format. In such a
> case, the time part is assumed to be 00:00:00Z (start of day, UTC).

Which means I have few options. I can use the far more cumbersome timestamp (ie
`2022-07-29T01:02:03-6.0`), I could deal with the date being wrong, I could update
the timestamp everytime I use it, or perhaps provide a PR to Contentlayer.

I dislike the middle idea, so I ended up creating a PR to contentlayer.
Essentially, this PR will update the date if a timezone is specified in the
config file and if the date that was already fetched does not match the offset
set specified in that timezone field of the config.

Here's the diff of my PR:

```diff
let dateValue = new Date(rawFieldData)
if (options.date?.timezone) {
- dateValue = dateFnsTz.zonedTimeToUtc(dateValue, options.date.timezone)
+  // NOTE offset of specified timezone in milliseconds
+  const desiredOffset = dateFnsTz.getTimezoneOffset(options.date.timezone)
+
+  // NOTE offset of raw date value is in minutes, we must multiple 60 then 1000 to get milliseconds
+  const currentOffset = dateValue.getTimezoneOffset() * 60 * 1000
+
+  if (desredOffset != currentOffset) {
+    dateValue = new Date(dateValue.getTime() + dateFnsTz.getTimezoneOffset(options.date.timezone) * -1)
+  }
}
```


### <span class="org-todo done DONE">DONE</span> A WIP {#a-wip}

As you can see this site is a serious WIP. But it's coming along, nonetheless --
and dare I say it, is actually useable. Few tasks I'd like to get done next
include:


#### <span class="org-todo todo TODO">TODO</span> RSS feed(s) {#rss-feed--s}


#### <span class="org-todo done DONE">DONE</span> Start rudimentary display of \`/grok\` -- aka the knowledge base {#start-rudimentary-display-of-grok-aka-the-knowledge-base}


#### <span class="org-todo todo TODO">TODO</span> Programmigcally display projects under \`/project\` based on GitHub and GitLab starred projects {#programmigcally-display-projects-under-project-based-on-github-and-gitlab-starred-projects}


#### <span class="org-todo todo TODO">TODO</span> Add a section for my JS and love.js games so they can all be played/explored from this site {#add-a-section-for-my-js-and-love-dot-js-games-so-they-can-all-be-played-explored-from-this-site}


#### <span class="org-todo todo TODO">TODO</span> Better logo/branding/header thing and a favicon that isn't the NextJS default {#better-logo-branding-header-thing-and-a-favicon-that-isn-t-the-nextjs-default}


#### <span class="org-todo todo TODO">TODO</span> Book review index as a gallery of book covers {#book-review-index-as-a-gallery-of-book-covers}


#### <span class="org-todo todo TODO">TODO</span> Typography improvements including a better font, better colors, and automatically swap quotes and dashes for the appropriate curly quote or emdash, etc. {#typography-improvements-including-a-better-font-better-colors-and-automatically-swap-quotes-and-dashes-for-the-appropriate-curly-quote-or-emdash-etc-dot}

I'd also really love to figure out a way to incorporate Git commits into each
post/review/etc. Basically figure out when the document was added in Git and
that would stand in as my \`date\` and then figure out last modified date/time and
add that as well. And speaking of time... the time values are wrong, I will need
to investigate what is up with the \`date-fns\` library I'm using.


### <span class="org-todo done DONE">DONE</span> Code Example {#code-example}

This is just an example of some code blocks for testing rehype prism.

This is the time parser I use for Planwarrior. As you can see on lines 1-3 I
have a time_str function which takes a string representation of time (`4:03`) and
then determines how many minutes that is since midnight:

```python
def time_str(s):
    h, m = [int(x) for x in s.split(':')]
    return h * 60 + m


def plan(plan):
    return {
        time_str((y := x.strip().split(' '))[0]): ' '.join(y[1:])
        for x in plan.strip().splitlines()
    }
```

Here are some utility functions I am using for the Planwarrior project:

```python
def peek_and_lookback(cur):
    prv = [None] + cur[:-1]
    nxt = cur[1:] + [None]
    return zip(prv, cur, nxt)


def peek(x):
    p = x[1:]
    p.append(None)
    return zip(x, p)


def wrap_ansi(s, code='green'):
    c = {
        'bold': ['\033[1m', '\033[00m'],
        'italic': ['\033[3m', '\033[00m'],
        'underline': ['\033[4m', '\033[00m'],
        'strike': ['\033[9m', '\033[00m'],
        'green': ['\033[0;32m', '\033[00m'],
    }
    d = 'green'
    return f"{c.get(code, d)[0]}{s}{c.get(code, d)[1]}"


def pad_maybe(i):
    return str(i) if len(str(i)) > 1 else f"0{i}"
```


### <span class="org-todo done DONE">DONE</span> ContentLayer Is Cool {#contentlayer-is-cool}

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


### <span class="org-todo done DONE">DONE</span> Hello World {#hello-world}


#### Hello! {#hello}

That's right, it's yet another blog from me. Another blog that, let's face it, I
won't probably use very much, and is really just an excuse to doing something
creative and learning a bit at the same time. Nonethesless, I am going to create
YAB and promise to update it.

My main motivations are:

-   Play with NextJS
-   Create a "digital garden" for my notes
-   In doing the above, hopefully eventually settle on platforms to use for day
    planning, notetaking, and writing

Unlike in prior blogs of mine (0x44.pw and bitranchlabs.com) I am going to try
to work on the development of this after my initial release. Meaning it's gonna
be ****ugly**** right now (and for a while)! I've also go to work on my neovim config
a bit, get my tmux-fu back up to speed, maybe get back to using
taskwarrior/timewarrior... so who knows, with so much stuff to do, maybe I'll
try writing about it some!
