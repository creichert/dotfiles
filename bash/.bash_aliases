# ~/.bash_aliases

alias ..="cd ../.."
alias ...="cd ../../.."
alias ....="cd ../../../.."
alias e="emacsclient -nw "
alias em="emacs -nw --color=never"
alias grep='grep --color=auto'
alias l='ls -CF'
alias la='ls -lah $LS_COLOR'
alias ll='ls -l'
alias ls='ls --color=auto'
alias m='make -j'
alias mi='make -j install'
alias s='stack'
alias upgrade='sudo apt update && sudo apt upgrade'
alias nil-uuid="echo 00000000-0000-0000-0000-000000000000"
alias gpg-agent-restart="gpg-connect-agent reloadagent /bye"
