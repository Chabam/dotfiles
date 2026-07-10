#!/bin/sh 
case $BLOCK_BUTTON in
    1) "$HOME"/.scripts/play-pause ;;  # right click, pause/unpause
esac

pangoify() {
    sed -e "s/\\&/&amp;/g;"
}

spotify=$(playerctl status)
if [ "$spotify" = "Playing" ] || [ "$spotify" = "Paused" ]; then
	if [ "$spotify" = "Paused" ]; then
		status="⏸️"
	else
		status="▶️"
	fi
	artist=$(playerctl metadata artist)
	title=$(playerctl metadata title)
	echo "$status $artist - $title" | pangoify
else
	echo ""
	exit
fi

