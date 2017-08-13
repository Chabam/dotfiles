#!/bin/sh

case "$(pidof spotify | wc -w)" in

0)	mpc stop &
	;;

*) 	playerctl stop &
	;;
esac