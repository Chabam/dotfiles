#!/bin/bash

# Author : Luke Smith
# Source : https://github.com/LukeSmithxyz/voidrice/blob/master/.scripts/i3volume

NUM=`cat /sys/class/power_supply/BAT0/capacity` || exit
STATUS=`cat /sys/class/power_supply/BAT0/status`

if [ "$NUM" -ge 80 ]; then
	color="#00FF00"
elif [ "$NUM" -ge 60 ]; then
	color="#FFFFFF"
elif [ "$NUM" -ge 40 ]; then
	color="#FFF600"
elif [ "$NUM" -ge 20 ]; then
	color="#FFAE00"
else
	color="#FF0000"
fi

[ "$STATUS" = "Charging" ] && color="#FFF"

echo "<span color='$color'>`sed -e "s/,//g;s/Discharging/🔋/;s/Charging/🔌/;s/Unknown/❓/;s/Full/⚡/;s/ 0*/ /g;s/ :/ /g" <<<"$STATUS"` `sed -e 's/$/%/' <<<"$NUM"`</span>"

