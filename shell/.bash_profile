[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"

export BROWSER="firefox"
export VISUAL="emacsclient -r"
export EDITOR="emacsclient -t"
export PAGER="less --use-color"

# Haskell
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# Rust
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export PATH="$PATH:$HOME/.local/bin:$HOME/.scripts"
