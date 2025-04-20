# ~/.bashrc:

set -o vi

[ -r ~/.bash_functions ]       && . ~/.bash_functions

[ -r ~/.bash_aliases   ] && . ~/.bash_aliases
#[ -r ~/.inputrc        ] && bind -f ~/.inputrc

[ -r /etc/bash_completion ] && . /etc/bash_completion
[ -x /usr/bin/lesspipe    ] && eval "$(SHELL=/bin/sh lesspipe)"

[ -f /usr/share/git/completion/git-prompt.sh ] && . /usr/share/git/completion/git-prompt.sh

[ -r ~/.dircolors ] \
    && eval "$(dircolors -b ~/.dircolors)" \
    || eval "$(dircolors -b)"

shopt -s histappend
shopt -s checkwinsize

export EDITOR=vim
export ALTERNATE_EDITOR="emacsclient -t"
export VISUAL=$EDITOR

export BROWSER=chromium
export GPG_TTY=$(tty)

HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups:erasedups
HISTSIZE=5000
HISTIGNORE=' *'


# Share history between all terminals immediately
PROMPT_COMMAND="history -a"
# PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

PS1_USER='\[\e[0;32m\]\u@\h\[[01m\]'
PS1_DIR='\[[0;35m\]\w\[[00m\]\[[1;30m\]\[[0;37m\]'
PS1_GITBRANCH='`__git_ps1 " (%s)"`\[[00m\]\[[0;37m\]'
PS1='\n'$PS1_USER':'$PS1_DIR$PS1_GITBRANCH'\n\$ '


# use node/npm using nvm by default
# bootstrap_lang node
