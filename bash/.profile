export DE="i3"
export SHELL="/bin/bash"
export BROWSER="firefox"
export EDITOR="nvim"
export PAGER="less"
export TERM="gnome-terminal"
export PATH="$PATH:$HOME/.scripts:$HOME/.scripts/private:$HOME/.cargo/bin"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
