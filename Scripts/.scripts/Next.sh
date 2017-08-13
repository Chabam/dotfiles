#!/bin/sh

case "$(pidof spotify | wc -w)" in

0)	mpc next &
	;;

*) 	playerctl next &
	;;
esac