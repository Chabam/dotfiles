#!/bin/sh
playerctl status
if [ $? -eq 1 ]; then
	mpc next
else
	playerctl next
fi