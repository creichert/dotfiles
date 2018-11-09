#!/bin/bash

# see `man xsession` & `man xsession.options` for documentation on how this file
# is used by X11.

set -e

feh --bg-scale ~/.xmonad/wallpaper

xset r rate 250 60             # speed up keys insert rate when held down
xset b off                     # turn bell off
xsetroot -cursor_name left_ptr # default cursor
setxkbmap -option ctrl:nocaps  # remap caps lock to ctrl

# Hold down right click to use trackball to scroll
xinput set-prop 8 "libinput Scroll Method Enabled" 0, 0, 1
xinput set-prop 8 "libinput Button Scrolling Button" 3

xmonad
