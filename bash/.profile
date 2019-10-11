export SHELL="/bin/bash"
export TERM="linux"
export BROWSER="google-chrome"
export EDITOR="nvim"
export PAGER="less"
export PATH="$PATH:$HOME/.scripts:$HOME/.scripts/private:"
export QT_QPA_PLATFORMTHEME="qt5ct"
if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi
