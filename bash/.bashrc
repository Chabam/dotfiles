# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
        if [ -f "$rc" ]; then
            . "$rc"
        fi
    done
fi
unset rc
# Disable Ctrl+S behavior
stty -ixon

GIT_PROMPT_SH=$SCRIPTS/git-prompt.sh

if [[ ! -f $GIT_PROMPT_SH ]]; then
	echo "Fetching git-prompt.sh into"
	pushd $SCRIPTS
	wget https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
	popd
fi

source git-prompt.sh

GIT_PS1_SHOWUPSTREAM="verbose"
GIT_PS1_DESCRIBE_STYLE="branch"
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWUNTRACKEDFILES=1
CLEAR="\[\e[0m\]"
GREEN="\[\e[1;92m\]"
BLUE="\[\e[1;94m\]"

GIT_PRE="$GREEN\u@\h$CLEAR:$BLUE\w$CLEAR"
GIT_POST="\$ "

# Fix for WSL for avoiding the use of git outside of the VM.
function print_git_prompt()
{
	if [[ "$(pwd)" =~ "/mnt" ]]; then
		PS1="$GIT_PRE$GIT_POST"
	else
		__git_ps1 "$GIT_PRE" "$GIT_POST"
	fi
}

PROMPT_COMMAND=print_git_prompt
PROMPT_DIRTRIM=3
force_color_prompt=yes
