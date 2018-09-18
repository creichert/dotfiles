#!/bin/bash

# ~/.bash_functions contains various utility functions, many of which
# are specific to my workflow, and is loaded from ~/.bashrc.


function git_delete_merged_branches() {
    git remote prune origin
    git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d
}

function background() {
    local CMD=$@
    local WORKSPACE=1234
    # -d starts a screen session and immediately detaches from it
    # -m forces creating a new screen session
    # -S lets you give the session a name
    # -L turns on logging to ~/screenlog.0
    screen -dmSL $WORKSPACE "$CMD"
    echo "backround session $WORKSPACE"
}

function background_attach() {
    local WORKSPACE=$1
    screen -x $WORKSPACE
}

function rm_old_linux_kernels() {
    dpkg -l linux-{image,headers}-* \
        | awk '/^ii/{print $2}' \
        | egrep '[0-9]+\.[0-9]+\.[0-9]+' \
        | grep -v $(uname -r | cut -d- -f-2) \
        | xargs sudo apt-get -y purge
}

function github_fetch_pr() {
    local PR_ID=$1
    local NEW_BRANCH=pr-$PR_ID
    git fetch origin pull/$PR_ID/head:$NEW_BRANCH
    git checkout $NEW_BRANCH
}

function make() {

    pathpat="(/[^/]*)+:[0-9]+"
    ccred=$(echo -e "\033[0;31m")
    ccyellow=$(echo -e "\033[0;33m")
    ccend=$(echo -e "\033[0m")
    /usr/bin/make "$@" 2>&1 | sed -E -e "/[Ee]rror[: ]/ s%$pathpat%$ccred&$ccend%g" -e "/[Ww]arning[: ]/ s%$pathpat%$ccyellow&$ccend%g"
    return ${PIPESTATUS[0]}
}

function rapid_keys() {
    xset r rate 250 60
}

function generate_password () {
    tr -dc A-Za-z0-9_ < /dev/urandom | head -c8
    echo
}

function find_largest_files () {
    #du -a . | sort -n -r | head -n 10
    du -a . | sort -n -r | head -n 10
}

# Print exe usage statistics
function freq_cmd() {
    local AWK='{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}'
    history | awk "$AWK"             \
            | grep -v "./"         \
            | column -c3 -s " " -t \
            | sort -nr             \
            | nl                   \
            | head -n10
}
