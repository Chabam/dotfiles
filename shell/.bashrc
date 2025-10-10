# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [ -f /etc/bash.bashrc ]; then
    . /etc/bash.bashrc
fi

if [[ "$TERM" == "dumb" ]]; then
   HISTFILE="$HOME/.tramp-histfile"
   return
fi

# Commands that should be applied only for interactive shells.
[[ $- == *i* ]] || return

# Stolen stuff from debian
PS1=$'\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

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

PROMPT_DIRTRIM=3

# Disable Ctrl+S behavior
stty -ixon

eval "$(direnv hook bash)"
