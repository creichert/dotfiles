# ~/.bash_aliases

alias ..="cd ../.."
alias ...="cd ../../.."
alias ....="cd ../../../.."
alias s="kitten ssh"
alias e="emacs -nw"
alias em="emacs -nw --color=never"
alias grep='grep --color=auto'
alias ls='ls --hyperlink=auto --color=auto'
alias l='ls -CF'
alias la='ls -lah'
alias ll='ls -l'
alias m='make -j'
alias nil-uuid="echo 00000000-0000-0000-0000-000000000000"

# other `gio trash` commands such as --list and --restore won't work without
# more of gnome being initialized.
alias clean_downloads="find downloads/ -type f -mtime +30 -exec gio trash {} \;"
alias empty_trash="find ~/.local/share/Trash -type f -delete"
alias zzz="systemctl poweroff"

# arch/pacman
alias arch_update="paru -Syu"
# paccache-hook already handles this automatically
alias arch_rm_orphans="paru -Qdtq | paru -R -"
alias arch_search_core="paru --searchby name --mode=r -Ss"

# nvidia
alias nvda="nvidia-smi"
