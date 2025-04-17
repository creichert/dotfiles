#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# Export path w/ stack bins
export PATH=${HOME}/.local/bin:$PATH

export SSH_AUTH_SOCK=${XDG_RUNTIME_DIR}/ssh-agent.socket

if uwsm check may-start; then
    exec uwsm start hyprland.desktop
fi

