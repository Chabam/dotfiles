#!/bin/sh

case "$(pidof spotify | wc -w)" in

0)	mpc prev &
	;;

*) 	playerctl previous &
	;;
esac