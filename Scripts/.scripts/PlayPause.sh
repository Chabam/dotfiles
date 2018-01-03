#!/bin/sh
playerctl status
if [ $? -eq 1 ]; then
	mpc toggle
else
	playerctl play-pause
fi
