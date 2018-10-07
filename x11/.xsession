#!/bin/bash

# see `man xsession` & `man xsession.options` for documentation on how this file
# is used by X11.

set -e

xset r rate 250 60
xset b off
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps

xmonad
