#!/bin/bash
video_url=$(printf "Cancel" | rofi -dmenu -p "Enter the video's URL")

if [[ "$video_url" != "cancel" && "$video_url" != "" ]]
then
    mpv $video_url
fi

