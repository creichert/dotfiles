#!/bin/bash

set -e

# [ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile   ] && . ~/.xprofile
[ -f ~/.xsession   ] && . ~/.xsession
[ -f ~/.Xdefaults  ] && xrdb -merge ~/.Xdefaults
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

# load theme and merge w/ .Xresources
xrdb -merge ~/.local/share/base16-xresources/xresources/base16-mocha-256.Xresources

xset r rate 250 60
xset b off
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps

# test if on laptop and set env var
if [ -d /proc/acpi/ibm ]; then

    export USING_LAPTOP="true"

else

    export USING_LAPTOP="false"
    xrandr --output DVI-D-1 --auto --right-of DVI-I-1
fi

exec xmonad
