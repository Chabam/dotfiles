CURRENT_THEME="$(gsettings get org.gnome.desktop.interface color-scheme)"
FOOT_PIDS=$(ps -x | grep foot | grep -v grep | awk '{print $1}')

if [[ "$CURRENT_THEME" == "'prefer-light'" ]]; then
    gsettings set org.gnome.desktop.interface color-scheme prefer-dark
    kill -s SIGUSR1 $FOOT_PIDS
    echo $'[main]\ninitial-color-theme=1' > ~/.config/foot/selected-theme.ini
else
    gsettings set org.gnome.desktop.interface color-scheme prefer-light
    kill -s SIGUSR2 $FOOT_PIDS
    echo $'[main]\ninitial-color-theme=2' > ~/.config/foot/selected-theme.ini
fi
