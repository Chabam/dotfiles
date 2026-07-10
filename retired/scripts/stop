#!/bin/sh
pkill -RTMIN+11 i3blocks
output=$(playerctl status)
if [ "$output" != "Playing" ] && [ "$output" != "Paused" ]; then
	mpc stop 2> /dev/null
else
	playerctl stop 2> /dev/null
fi
