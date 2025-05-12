# ~/.bashrc:

set -o vi

[ -r ~/.bash_functions ]       && . ~/.bash_functions

[ -r ~/.bash_aliases   ] && . ~/.bash_aliases
[ -r ~/.inputrc        ] && bind -f ~/.inputrc

[ -r /etc/bash_completion ] && . /etc/bash_completion
[ -x /usr/bin/lesspipe    ] && eval "$(SHELL=/bin/sh lesspipe)"

[ -f /usr/share/git/completion/git-prompt.sh ] && . /usr/share/git/completion/git-prompt.sh

[ -r ~/.dircolors ] \
    && eval "$(dircolors -b ~/.dircolors)" \
    || eval "$(dircolors -b)"

shopt -s histappend
shopt -s checkwinsize

export EDITOR=vim
export ALTERNATE_EDITOR="emacs -nw"
export VISUAL=$EDITOR

export BROWSER=chromium
export GPG_TTY=$(tty)

HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
HISTSIZE=10000
HISTIGNORE=' *'

# Share history between all terminals immediately
PROMPT_COMMAND="history -a"
# PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"

PS1_USER='\[\e[0;32m\]\u@\h\[[01m\]'
PS1_DIR='\[[0;35m\]\w\[[00m\]\[[1;30m\]\[[0;37m\]'
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWDIRTYSTATE=1
PS1_GITBRANCH='`__git_ps1 " (%s)"`\[[00m\]\[[0;37m\]'
PS1='\n'$PS1_USER':'$PS1_DIR$PS1_GITBRANCH'\n\$ '


# export LESS_TERMCAP_mb=$'\e[01;31m'       # begin blinking
# export LESS_TERMCAP_md=$'\e[01;38;5;74m'  # begin bold
# export LESS_TERMCAP_me=$'\e[0m'           # end mode
# export LESS_TERMCAP_se=$'\e[0m'           # end standout-mode
# export LESS_TERMCAP_so=$'\e[38;5;246m'    # begin standout-mode - info box
# export LESS_TERMCAP_ue=$'\e[0m'           # end underline
# export LESS_TERMCAP_us=$'\e[04;38;5;146m' # begin underline
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4) # yellow on blue
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT="-P -c"

# use node/npm using nvm by default
# bootstrap_lang node