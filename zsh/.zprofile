if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
	while true; do
	echo "1) XFCE 2) BSPWM"
	read choice
	case $choice in
		1 ) exec startx ~/.xinitrc xfce4; break;;
		2 ) exec startx ~/.xinitrc bspwm break;;
		* ) echo "Not an answer";;
	esac
done
fi
