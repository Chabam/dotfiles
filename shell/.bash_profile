export BROWSER="firefox"
export VISUAL="emacsclient -c"
export EDITOR="nvim"
export PAGER="less --use-color"
export PATH="$HOME/.local/bin:$PATH"

# Adding environment variables set here to systemd user services
dbus-update-activation-environment --systemd --all
