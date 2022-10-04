+++
title = "Setting Up A Firewall"
author = ["Ian S. Pringle"]
date = 2022-09-02T09:50:00-05:00
tags = ["infra"]
draft = false
+++

## Introduction {#introduction}

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
GitOps. You can checkout the literate server setup [here]({{< relref "#d41d8c" >}}).

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


## ferm {#ferm}

First thing's first, I checked out the example configs that come with the
default Ubuntu install of `ferm` and then cat'd together the ones I was interested
in ~~ripping off~~ using. The three I thought looked most useful are
`webserver.ferm`, `antiddos.ferm`, and `ipv6.ferm`.


### Examples {#examples}


#### webserver.ferm {#webserver-dot-ferm}

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


#### antiddos.ferm {#antiddos-dot-ferm}

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


#### ipv6.ferm {#ipv6-dot-ferm}

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


### The Configuration {#the-configuration}

ferm's configuration is pretty straightforward, you tables and chains, you can
specific a domain (ip, ipv6) for the table, and you can target specific
predefined policies (ACCEPT, DROP, etc.). I am not a networking guy or a
firewall guru, but I think I have enough understanding to write something...


#### The filter Table {#the-filter-table}

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


#### Anti-DDoS logic {#anti-ddos-logic}

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


#### IPv6 {#ipv6}

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


#### Putting It Together {#putting-it-together}

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


## Conclusion {#conclusion}

I will likely be playing with this for a while and change things to adapt with
time. I would like to get better observability into what is being hit, by whom,
etc. and when I have that I can review incoming requests, pick out problematic
patterns and work to shut them down. This will mean changes to the firewall. My
ever-up-to-date firewall configuration can be found in my [literate server
configuration]({{< relref "#d41d8c" >}}) under the firewall heading. In the future, I would like to also
add fail2ban to my setup, however I have [read](https://tina.pm/blog/posts/Setting_up_my_server:_netfilter/#comments) something about how ferm can wipe
out f2b's tables and I want to research into this some more before I incorporate
it into my setup. Another idea I had was to use ferm to create the iptables
values and then directly insert them into iptables. This might or might not be a
good idea, but it's something I might look into.
