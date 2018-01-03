#!/bin/sh
playerctl status
if [ $? -eq 1 ]; then
	mpc prev
else
	playerctl previous
fi