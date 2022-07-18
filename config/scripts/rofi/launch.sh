#!/bin/bash

menu="$1"

if [ "$menu" = "appmenu" ]; then
    rofi -show drun

elif [ "$menu" = "powermenu" ]; then
    rofi -modi 'Powermenu:~/.config/scripts/rofi/powermenu.sh' -show Powermenu -theme powermenu -location 3 -xoffset -30 -yoffset 100
fi
