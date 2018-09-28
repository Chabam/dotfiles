#!/bin/sh
pkill -RTMIN+11 i3blocks
output=$(playerctl status)
if [ "$output" != "Playing" ] && [ "$output" != "Paused" ]; then
	mpc toggle 2> /dev/null
else
	playerctl play-pause 2> /dev/null
fi
