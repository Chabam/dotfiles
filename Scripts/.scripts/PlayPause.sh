#!/bin/sh

case "$(pidof spotify | wc -w)" in

0)	mpc toggle
	;;

*) 	playerctl play-pause &
	;;
esac