export EDITOR="nvim"
export VISUAL="nvim"
export PATH="$PATH:$HOME/.scripts:$HOME/.cargo/bin"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
