# ~/.bash_aliases

alias ..="cd ../.."
alias ...="cd ../../.."
alias ....="cd ../../../.."
alias s="kitten ssh"
alias e="emacs -nw"
alias em="emacs -nw --color=never"
alias grep='grep --color=auto'
alias l='ls -CF'
alias la='ls -lah $LS_COLOR'
alias ll='ls -l'
alias ls='ls --color=auto'
alias m='make -j'
alias nil-uuid="echo 00000000-0000-0000-0000-000000000000"

alias empty_trash="find ~/.local/share/Trash -type f -delete"
alias zzz="systemctl poweroff"

# arch/pacman
alias arch_cache_clean="sudo pacman -Scc"
alias arch_rm_orphans="sudo pacman -Qdtq | sudo pacman -R -"
alias arch_update="sudo pacman -Syu"

# nvidia
alias nvda="nvidia-smi"
