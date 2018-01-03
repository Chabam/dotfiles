#!/bin/sh
playerctl status
if [ $? -eq 1 ]; then
	mpc stop
else
	playerctl stop
fi