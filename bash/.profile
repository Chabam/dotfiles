export EDITOR="vim"
export PATH=:"$PATH:~/.scripts"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
