# ~/.bash_aliases

alias ..="cd ../.."
alias ...="cd ../../.."
alias ....="cd ../../../.."
alias distupgrade='sudo apt-get update && sudo apt-get dist-upgrade'
alias e="emacsclient -t"
alias emacs='emacs -nw'
alias emcas='emacs -nw'
alias em="emacs -nw"
alias gnus='emacs -f gnus'
alias grep='grep --color=auto'
alias l='ls -CF'
alias la='ls -lah $LS_COLOR'
alias ll='ls -l'
alias ls='ls --color=auto'
alias m='make -j || stack build || cabal build'
alias mi='make -j install'
alias s='stack'
alias upgrade='sudo apt-get update && sudo apt-get upgrade'
alias xml="xmlstarlet format"
