# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
export TEXINPUTS="~/Cours/Common/Latex:"
export LATEXINDENT_CONFIG="~/.config/latexindent.yaml"
export SCRIPTS="~/.scripts"

export BROWSER="firefox"
export EDITOR="nvim"
export PAGER="less"
export SCRIPTS="$HOME/.scripts"
export SCRIPTS_PRIVATE="$HOME/.scripts/private"
export RUST_BIN="$HOME/.cargo/bin"
export PATH="$PATH:$SCRIPTS:$SCRIPTS_PRIVATE:$RUST_BIN"
export DOCKER_HOST=unix:///run/user/1000/docker.sock

export TEXINPUTS="$HOME/Cours/common/latex:"
