#!/bin/bash

#variables
pidfile="/var/run/user/$UID/polybar-mic-script.pid"

#Colors
COLOR_UNMUTED="%{F#00FF00}"
COLOR_MUTED="%{F#de0000}"

#icon format
micON="${COLOR_UNMUTED}"
micOFF="${COLOR_MUTED}"


if [ -e "$pidfile" ]; then
	kill "$(cat "${pidfile}")" > /dev/null 2>&1
	cleanup
fi

cleanup() {
	rm -f "$pidfile"
}

#Return(with echo) the status of the microphone
print_status() {
	if amixer get Capture | grep -q '\[off\]'; then
		echo $micOFF${COLOR_MUTED}
	else
		echo $micON${COLOR_UNMUTED}
	fi
}

#Toggle the microphone status then update it with the print_status func
toggle() {
	amixer set Capture toggle > /dev/null 2>&1
	print_status
}

#Get the inputs
trap "cleanup" EXIT
trap "toggle" USR1

echo $$ > "$pidfile"

### The "main" func
while true; do
	print_status
	sleep 0.5 &
	wait
done
