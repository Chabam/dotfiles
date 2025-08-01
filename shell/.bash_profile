export BROWSER="firefox"
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -t"
export PAGER="less --use-color"

[ -f "/home/chabam/.ghcup/env" ] && . "/home/chabam/.ghcup/env" # ghcup-env

export PATH="$PATH:$HOME/.local/bin"

# Adding environment variables set here to systemd user services
dbus-update-activation-environment --systemd --all
