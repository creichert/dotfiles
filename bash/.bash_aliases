# ~/.bash_aliases

alias ..="cd ../.."
alias ...="cd ../../.."
alias ....="cd ../../../.."
alias e="emacs -nw"
alias em="emacs -nw --color=never"
alias grep='grep --color=auto'
alias l='ls -CF'
alias la='ls -lah $LS_COLOR'
alias ll='ls -l'
alias ls='ls --color=auto'
alias m='make -j'
alias mi='make -j install'
alias nil-uuid="echo 00000000-0000-0000-0000-000000000000"

# arch/pacman
alias pacman_cache_clean="sudo pacman -Scc"
alias pacman_remove_orphans="sudo pacman -Qdtq | sudo pacman -R -"
alias pacman_update="sudo pacman -Syu"
