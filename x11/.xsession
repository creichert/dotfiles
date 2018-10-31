#!/bin/bash

# see `man xsession` & `man xsession.options` for documentation on how this file
# is used by X11.

set -e

xflux -z 77006
feh --bg-scale ~/.xmonad/wallpaper

xset r rate 250 60
xset b off
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps

# Hold down right click to use trackball to scroll
xinput set-prop 8 "libinput Scroll Method Enabled" 0, 0, 1
xinput set-prop 8 "libinput Button Scrolling Button" 3

/usr/lib/notification-daemon/notification-daemon &

xmonad
