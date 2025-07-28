# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [[ "$TERM" == "dumb" ]]; then
   HISTFILE="$HOME/.tramp-histfile"
   return
fi


# Commands that should be applied only for interactive shells.
[[ $- == *i* ]] || return

HISTCONTROL=erasedups
HISTFILESIZE=100000
HISTIGNORE=l:ls:cd:exit
HISTSIZE=10000

shopt -s histappend
shopt -s checkwinsize
shopt -s extglob
shopt -s globstar
shopt -s checkjobs

alias ll='ls -l'
alias lla='ls -la'

force_color_prompt=yes
PROMPT_DIRTRIM=3

# Disable Ctrl+S behavior
stty -ixon

eval "$(direnv hook bash)"
