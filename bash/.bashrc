# ~/.bashrc:

set -o vi

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
[ -x /usr/bin/lesspipe    ] && eval "$(SHELL=/bin/sh lesspipe)"

[ -r ~/.bash_functions ] && . ~/.bash_functions
[ -r ~/.bash_aliases   ] && . ~/.bash_aliases
[ -r ~/.inputrc        ] && bind -f ~/.inputrc

[ -f /usr/share/git/completion/git-prompt.sh ] && . /usr/share/git/completion/git-prompt.sh

[ -r ~/.dircolors ] \
    && eval "$(dircolors -b ~/.dircolors)" \
    || eval "$(dircolors -b)"

shopt -s histappend
shopt -s checkwinsize

# Generous interactive history with bounded disk retention.
export HISTSIZE=10000
export HISTFILESIZE=200000

HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
HISTIGNORE=' *'
HISTTIMEFORMAT='%F %T '

# Persist commands immediately, without reloading all terminal history.
PROMPT_COMMAND="history -a"

bind 'set completion-ignore-case on'

export EDITOR=vim
export ALTERNATE_EDITOR="emacs -nw"
export VISUAL=$EDITOR

export BROWSER=chromium
export GPG_TTY=$(tty)

export PASSWORD_STORE_ENABLE_EXTENSIONS=true

PS1_USER='\[\e[0;32m\]\u@\h\[\e[01m\]'
PS1_DIR='\[\e[0;35m\]\w\[\e[00m\]\[\e[1;30m\]\[\e[0;37m\]'
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWDIRTYSTATE=1
PS1_GITBRANCH='`__git_ps1 " (%s)"`\[\e[00m\]\[\e[0;37m\]'
PS1='\n'$PS1_USER':'$PS1_DIR$PS1_GITBRANCH'\n\$ '