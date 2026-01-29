[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"

export BROWSER="firefox"
export PAGER="less --use-color"
if [ -f /usr/bin/toolbox ];
then
    export VISUAL="toolbox run -c emacs emacsclient -c"
else
    if [ -f /usr/bin/emacs ]; then
        export VISUAL="emacs emacsclient -c"
    else
        export VISUAL="vi"
    fi
fi

export EDITOR="$VISUAL"

# Haskell
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# Rust
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export PATH="$PATH:$HOME/.local/bin:$HOME/.scripts"
