export DE="i3"
export TERM="urxvt"
export SHELL="bash"
export BROWSER="firefox"
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"
export PATH="$PATH:$HOME/.scripts:$HOME/.cargo/bin"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
