+++
title = "Server"
author = ["Ian S. Pringle"]
draft = false
+++

## About {#about}

This is the _literate_ file that defines a server. All of the source files for Lit
Server are defined here and are "tangled" together with `org-tangle`. This is the
only file that should ever be edited (other than the README).


## README.org {#readme-dot-org}

It seems a little weird to build an org file, with an org file. I'm really only
doing this because the `server` directory is entry built from this org file, but
the intention is that I compile the files in `server` locally and commit them.
There is a chance that someone -- including future me -- might come upon this
`server` directory and be uninformed about it's creation and use.

```org
#+title: Readme

NOTICE: This file and all files in this directory were built with =server.org=
and should not be directly edited!

This is the server dir used to deploy my bitranchlabs server infra.
```


## Deploy {#deploy}

There are a number of ways that this could be deployed. I think I will
eventually settle on gitlab-ci, but incase I want to deploy from local I have
come up with a solution using the package `(emacs-ssh-deploy)`.


### deploy.sh {#deploy-dot-sh}

This is a simple shell script to deploy my server. It assumes all the files that
are needed can be found in the remote path `/opt/lit-server` and it needs to be
run as root.

```bash
cp /home/ian/lit-server/Caddyfile /etc/caddy/Caddyfile
cp /home/ian/lit-server/docker-compose.yml /opt/composed/docker-compose.yml
cd /opt/composed/
docker-compose up -d
systemctl reload caddy
```


### Makefile {#makefile}

Emacs can run make files, that might be enough for local deployments... This
sort of works actually, but the `deploy-remote` stuff seems to just always fail
and then I have to ssh into the machine and run that exact same command. Sort of
ridiculous tbh...

Caddy has a `--watch` flag, might be worth looking at using that to watch for
changes to the config. But it could also be a disaster, I'm not sure...

```makefile
.PHONE: all deploy deploy-local install sync setup, clean
all: sync deploy-remote
clean:
		ssh ian@server-01.bitranclabs.net rm -rf ~/lit-server
deploy-local:
		cp /home/ian/lit-server/Caddyfile /etc/caddy/Caddyfile
		cp /home/ian/lit-server/docker-compose.yml /opt/composed/docker-compose.yml
		cd /opt/composed/
		docker-compose up -d
		systemctl reload caddy
deploy-remote:
		ssh root@server-01.bitranchlabs.net bash /home/ian/lit-server/deploy.sh
install: deploy-remote
setup:
		ssh ian@server-01.bitranchlabs.net mkdir -p /home/ian/lit-server
		ssh root@server-01.bitranchlabs.net mkdir -p /opt/composed
sync: setup
		@echo "Publishing to remote"
		rsync -chazve ssh ~/org/server/ ian@server-01.bitranchlabs.net:~/lit-server/
```


### emacs-ssh-deploy <span class="tag"><span class="ARCHIVE">ARCHIVE</span></span> {#emacs-ssh-deploy}


## Caddy {#caddy}

We're going to try using Caddy for the server for this. I was using Traefik in
docker and it was just crapping out, so I gave up after much frustration and
decided to put the server _outisde_ of Docker. Caddy is a very straightforward
server to setup but has a lot of features. Just an aside, I'm using "shell" for
the src type on these blocks because org doesn't know what a `Caddyfile` type is,
and shell and Caddy share the same comment types (ie `#` ).


### Globals {#globals}

Here are the global defaults for Caddy. Since Caddy comes with TLS by default,
all we have to do is give it an email address:

```shell
{
    email ian@dapringles.com
}
```

These aren't exactly "global" but they're some defaults that'll be used with
mosta/all subsequent server configurations. First up "encoding", some of these
settings are the defaults, I am adding them for visibility:

```shell
(encoding) {
  encode {
    zstd
    gzip
  }
}
```

Some default headers we want to upstream:

```shell
(upheaders) {
    header_up X-Forwarded-Ssl on
    header_up X-Real-IP {remote}
    header_up X-Forwarded-Port {server_port}
    header_up X-Forwarded-Proto {scheme}
    header_up X-Url-Scheme {scheme}
    header_up X-Forwarded-Host {host}
}
```


### Services {#services}

For the most part, the services Caddy is going to be serving are defined in the
section on each server and then `noweb` will tangle them all into the Caddyfile
for us.


#### File Server {#file-server}

I think it would be nice to have a fileserver for anything I want to share or access. I'm not sure I will keep this up but it's here for the time being.

This isn't working right now actually...

<a id="code-snippet--fileserver-caddy"></a>
```bash
files.bitranchlabs.net {
    file_server browse {
        root * /static
        hide .git
        precompressed zstd br gzip
        import upheaders
    }
    import encoding
}
```


## Services {#services}

One thing of note here, since we're exposing all the docker containers to
localhost, we have to keep track of their ports or else there could be a
collision and something will not be happy.


### Boilerplate {#boilerplate}

We'll be using docker-compose for most services.

```yaml
version: '3.7'
services:
```


### whoami {#whoami}

This is a pretty worthless service beyond just basic troubleshooting and sanity
checking. It'll also serve as something of a "template" for creating other
services I guess.


#### docker-compose.yaml {#docker-compose-dot-yaml}

First we need docker-compose file:

```yaml
  whoami:
    image: docker.io/containous/whoami:latest
    restart: always
    ports:
      - 8000:80
```


#### Caddyfile {#caddyfile}

This is the definition of this service in our Caddyfile:

<a id="code-snippet--whoami-caddy"></a>
```bash
whoami.bitranchlabs.net {
    reverse_proxy http://127.0.0.1:8000 {
        import upheaders
    }
    import encoding
}
```


### Resume site {#resume-site}

This is my resume website, usually it can be found at <https://ianpringle.org/>.
It can also be found at <https://resume.bitranchlabs.net>. No real reason for
both, just something I like to do I guess...


#### docker-compose.yaml {#docker-compose-dot-yaml}

Make sure there is no collision on local host and then match that with port 80
inside the container:

```yaml
  resume:
    image: registry.gitlab.com/pard/resume-site:latest
    restart: always
    ports:
      - 8001:80
```


#### Caddyfile {#caddyfile}

This is the definition of this service in our Caddyfile:

<a id="code-snippet--resume-caddy"></a>
```bash
resume.bitranchlabs.net ianpringle.org {
    reverse_proxy http://127.0.0.1:8001 {
        import upheaders
    }
    import encoding
}
```


### Ungovernable World {#ungovernable-world}


#### docker-compose.yaml {#docker-compose-dot-yaml}

Make sure there is no collision on local host and then match that with port 80
inside the container:

```yaml
  ungovernable-world:
    image: docker.io/pard68/ungovernable
    restart: always
    ports:
      - 8002:80
```


#### Caddyfile {#caddyfile}

This is the config for the actual site, we put the redirect block above it so
that it can be properly redirected.

<a id="code-snippet--ungovernable-world-caddy"></a>
```bash
ungovernable.world {
    reverse_proxy http://127.0.0.1:8002 {
        import upheaders
    }
    import encoding
}
```


### Bitranchlabs {#bitranchlabs}


#### Blog {#blog}

<!--list-separator-->

-  docker-compose.yaml

    Make sure there is no collision on local host and then match that with port 80
    inside the container:

    ```yaml
      brl-www:
        image: registry.gitlab.com/bitranchlabs/www:latest
        restart: always
        ports:
          - 8003:80
    ```

<!--list-separator-->

-  Caddyfile

    This is for redirecting some URLs to the desired site.

    <a id="code-snippet--brl-www-redir"></a>
    ```bash
    bitranchlabs.com, bitranchlabs.net, www.bitranchlabs.net, bitranchlabs.org, www.bitranchlabs.org {
        redir https://bitranchlabs.com{uri}
    }
    ```

    This is the config for the actual site, we put the redirect block above it so
    that it can be properly redirected.

    <a id="code-snippet--brl-www-caddy"></a>
    ```bash
    www.bitranchlabs.com {
        reverse_proxy http://127.0.0.1:8003 {
            import upheaders
        }
        import encoding
    }
    ```


#### API {#api}

I use the `bitranchlabs.com` domain for some APIs. Because I route by path for these, the setup can be a bit complex (it isn't currently, but if I add API that'll change). In Caddy we will need to use a directive called `handle` to manage directing ports by path. These directives need to all be in the same block, so we'll use some noweb for that.

<a id="code-snippet--brl-api-caddy"></a>
```shell
api.bitranchlabs.com {
    <<brl-your-face-api>>
    import encoding
}
```

<!--list-separator-->

-  Your Face API

    <!--list-separator-->

    -  docker-compose.yaml

        Make sure there is no collision on local host and then match that with port 80
        inside the container:

        ```yaml
          brl-your-face-api:
            image: registry.gitlab.com/pard/yourface-api:master-1
            restart: always
            ports:
              - 8004:80
        ```

    <!--list-separator-->

    -  Caddyfile

        This maps that path specified to the port. We are using Caddy's `handle_path` directive here because we want to strip that path being requested since the upstream microserve isn't aware of paths and thinks it's being served from `/`

        <a id="code-snippet--brl-your-face-api"></a>
        ```bash
            handle_path /api/v1/yourface {
                reverse_proxy http://127.0.0.1:8004 {
                    import upheaders
                }
            }
        ```


## Firewall {#firewall}

ferm's configuration is pretty straightforward, you tables and chains, you can
specific a domain (ip, ipv6) for the table, and you can target specific
predefined policies (ACCEPT, DROP, etc.). I am not a networking guy or a
firewall guru, but I think I have enough understanding to write something...

```shell
<<anti-ddos-func>>
```


### The filter Table {#the-filter-table}

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

```:tangle
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
<<anti-ddos-func-call>>
    }
    chain OUTPUT policy ACCEPT;
    chain FORWARD policy DROP;
}
```


### Anti-DDoS logic {#anti-ddos-logic}

To mitigate DDoS attacks, we can define a function to track requests over time
for a specfic IP address and if it exceeds a given threshold, we block that
address for some length of time. This comes right from the examples document,
except I removed the IP address exceptions logic because I don't have a static
address and the address I do get is sometimes shared with other Starlink
customers.

<a id="code-snippet--anti-ddos-func"></a>
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

<a id="code-snippet--anti-ddos-func-call"></a>
```shell
        &ANTIDDOS((22 80 443), 50, 3, 86400);
```


### IPv6 {#ipv6}

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


## Utilities {#utilities}


### (org-tangle-into-dir) {#org-tangle-into-dir}

This is a helper function to make tangling a little simpler. This is also in my
emacs config, but it's here because if this file is to be run on by someone else
or with a minimal config, this function would be required still. I got this from
[here](https://emacs.stackexchange.com/questions/46479/how-to-set-a-tangled-parent-directory-for-each-subtree-in-org-mode).

```emacs-lisp
(defun org-tangle-into-dir (sub-path)
  "Expand the SUB-PATH into the directory given by the tangle-dir
   property if that property exists, else use the
   `default-directory'."
  (expand-file-name sub-path
                    (or
                     (org-entry-get (point) "tangle-dir" 'inherit)
                     (default-directory))))
```
