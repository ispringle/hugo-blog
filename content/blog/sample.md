+++
title = "Literate Dots Part 1: Zsh!"
author = ["Ian S. Pringle"]
tags = ["general"]
draft = false
+++

I had mentioned [tangling all the things]({{< relref "#d41d8c" >}}) yesterday, and I've gone ahead and
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


## Environment Variables {#environment-variables}

Sadly, ZSH sorta sucks and the `.zshenv` has to live in the home directory, so
we're going to set that up and tell it to look in `.config/zsh/` for all the
rest of the zsh config files we might use. So we'll setup the zshenv file to
have all the right XDG settings, plus point all the various other things that
need to be told to use .config to do so.


### XDG and Zsh paths {#xdg-and-zsh-paths}

```sh
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_HOME="$HOME/.cache/"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export HISTFILE="$ZDOTDIR/zhistory"
```


### HIST {#hist}

We want to make sure our history is nice and long. It's 2022, so we shouldn't
worry too much about disk space or RAM... I hope

```sh
export HISTSIZE=10000
export SAVEHIST=10000
```


### EDITOR {#editor}

Let's set our editor quickly so we can make sure we're never far away from
emacs.

```sh
export EDITOR="/usr/local/bin/emacsclient"
export VISUAL="/usr/local/bin/emacsclient"
```


### $PATH {#path}

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


## Zshrc {#zshrc}


### A E S T H E T I C {#a-e-s-t-h-e-t-i-c}

Your terminal, in Technicolor!

```sh
autoload -U colors && colors
PROMPT="%B%F{magenta}λ%f%b "
RPROMPT="%*"
```


### Zsh Options {#zsh-options}

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


### Ghetto Jump {#ghetto-jump}

There are some neat "jump" plugins like `j` and `z`. But we're just going to DIWhy it!

```sh
alias d='dirs -v'
for index ({1..9}) alias "$index"="cd + ${index}"; unset index
```


### Completion {#completion}

```sh
autoload -U compinit
zstyle ':completion:*' menu select completer _complete _correct _approximate
zmodload zsh/complist
compinit
_comp_options+=(globdots)
```


## Aliases {#aliases}

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
