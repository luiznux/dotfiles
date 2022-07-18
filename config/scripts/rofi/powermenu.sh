#!/bin/bash


if [ -z "$@" ]; then
    echo -en "Hibernate\0icon\x1fsystem-suspend-hibernate\n"
    echo -en "Shutdown\0icon\x1fsystem-shutdown\n"
    echo -en "Reboot\0icon\x1fsystem-reboot\n"
    echo -en "Lock\0icon\x1fsystem-lock-screen\n"
    echo -en "Logout\0icon\x1fsystem-log-out\n"

else
    if [ "$1" = "Shutdown" ]; then
        shutdown now
    elif [ "$1" = "Logout" ]; then
        i3-msg exit
    elif [ "$1" = "Reboot" ]; then
        reboot
    elif [ "$1" = "Lock" ]; then
        i3lock --nofork
        #sudo ~/.local/bin/betterlockscreen -l blur
    elif [ "$1" = "Hibernate" ]; then
        systemctl hibernate
    fi
fi
