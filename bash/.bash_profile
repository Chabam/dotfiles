# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
export SCRIPTS="~/.scripts"

export BROWSER="firefox"
export EDITOR="emacs"
export PAGER="less"
export SCRIPTS="$HOME/.scripts"
export SCRIPTS_PRIVATE="$HOME/.scripts/private"
export RUST_BIN="$HOME/.cargo/bin"
export PATH="$PATH:$SCRIPTS:$SCRIPTS_PRIVATE:$RUST_BIN"
