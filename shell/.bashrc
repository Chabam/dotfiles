# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [ -f /etc/bash.bashrc ]; then
    . /etc/bash.bashrc
fi

if [ -f /etc/profile.d ]; then
    source /etc/profile.d/*.sh
fi

# Commands that should be applied only for interactive shells.
[[ $- == *i* ]] || return

if [ -f "/run/.toolboxenv" ]
then
    TOOLBOX_NAME=$(sed -n 's/.*name="\([^"]*\)".*/\1/p' /run/.containerenv)
    TOOLBOX_PROMPT="\[\033[35m\]⬢ ${TOOLBOX_NAME}\[\033[00m\] "
fi

# Stolen stuff from debian
export PS1="${TOOLBOX_PROMPT}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\$ "

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='TERM=ansi ls --color=auto'
    alias dir='TERM=ansi dir --color=auto'
    alias vdir='TERM=ansi vdir --color=auto'

    alias grep='TERM=ansi grep --color=auto'
    alias fgrep='TERM=ansi fgrep --color=auto'
    alias egrep='TERM=ansi egrep --color=auto'
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

# Disable Ctrl+S behavior
stty -ixon

if [ -f /usr/bin/direnv ]; then
    eval "$(direnv hook bash)"
fi
