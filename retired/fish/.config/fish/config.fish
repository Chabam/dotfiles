if status is-interactive
    # Commands to run in interactive sessions can go here
    set -g fish_greeting ""
    source ~/.config/fish/functions/vte_integration.fish
end


export SHELL="/bin/fish"
export BROWSER="firefox"
export EDITOR="nvim"
export PAGER="less"
export SCRIPTS="$HOME/.scripts"
export SCRIPTS_PRIVATE="$HOME/.scripts/private"
export RUST_BIN="$HOME/.cargo/bin"
export LOCAL_BIN="$HOME/.local/bin/"
export PATH="$PATH:$SCRIPTS:$SCRIPTS_PRIVATE:$RUST_BIN:$LOCAL_BIN"

export TEXINPUTS="$HOME/Cours/common/latex:"
