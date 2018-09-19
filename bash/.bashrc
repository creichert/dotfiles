# ~/.bashrc:

set -o vi

[ -r ~/.bash_functions ]       && . ~/.bash_functions

# private bash functions
[ -r ~/.bash_functions.local ] && . ~/.bash_functions.local

[ -r ~/.bash_aliases   ] && . ~/.bash_aliases
[ -r ~/.inputrc        ] && bind -f ~/.inputrc

[ -r /etc/bash_completion ] && . /etc/bash_completion
[ -x /usr/bin/lesspipe    ] && eval "$(SHELL=/bin/sh lesspipe)"

[ -r ~/.dircolors ] \
    && eval "$(dircolors -b ~/.dircolors)" \
    || eval "$(dircolors -b)"

shopt -s histappend
shopt -s checkwinsize

export ALTERNATE_EDITOR="emacsclient -t -nw"
export EDITOR="emacsclient -t -nw"
export GIT_EDITOR="emacsclient -t -nw"
export SYSTEMD_EDITOR="emacsclient -t -nw"
export VISUAL=$EDITOR

export BROWSER=chromium
export TERM=xterm
export GPG_TTY=$(tty)

HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
HISTFILESIZE=1000000
HISTSIZE=1000000

# Share history between all terminals immediately
PROMPT_COMMAND='history -a'

PS1_USER='\[\e[0;32m\]\u@\h\[[01m\]'
PS1_DIR='\[[0;35m\]\w\[[00m\]\[[1;30m\]\[[0;37m\]'
PS1_GITBRANCH='`__git_ps1 " (%s)"`\[[00m\]\[[0;37m\]'
PS1='\n'$PS1_USER':'$PS1_DIR$PS1_GITBRANCH'\n\$ '

export NIL_UUID=00000000-0000-0000-0000-000000000000

# Nodejs
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ]          && . "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Rust
export RSVM_DIR="$HOME/.rsvm"
if [[ -s "$RSVM_DIR/rsvm.sh" ]]; then
    . "$RSVM_DIR/rsvm.sh"
else
    echo "rsvm not installed."
fi

# Go
export GOVM_ROOT="$HOME/.govm"
if [[ -d "$GOVM_ROOT" ]]; then
    export PATH="$GOVM_ROOT/bin:$PATH"
    export PATH="$GOVM_ROOT/versions/current/bin:$PATH"
else
    echo "govm not installed."
fi

# Export path w/ stack bins
export PATH=${HOME}/.local/bin:`stack path --compiler-bin`:$PATH
eval "$(stack --bash-completion-script stack)"