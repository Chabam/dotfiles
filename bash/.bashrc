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

gitstatus_stop && gitstatus_start -s -1 -u -1 -c -1 -d -1 -m -1
PROMPT_COMMAND=__prompt_command

__prompt_command() {
  if [[ -n "$IN_NIX_SHELL" ]] && [[ -z $ORIG_SHLVL ]] then
    export ORIG_SHLVL=$SHLVL
  fi

  EXIT_CODE=$?
  CLEAR="\[\e[0m\]"
  RED="\[\e[1;31m\]"
  GREEN="\[\e[1;32m\]"
  YELLOW="\[\e[1;33m\]"
  BLUE="\[\e[1;34m\]"
  CYAN="\[\e[1;36m\]"
  PURPLE="\[\e[0;35m\]"


  PS1=""

  HOST_INFO=""
  if [[ $SSH_TTY ]]; then
    HOST_INFO="$CLEAR at $YELLOW\h$CLEAR"
  fi

  PS1+="$BLUE\w$CLEAR$HOST_INFO"

  GIT_INFO=""
  if gitstatus_query && [[ "$VCS_STATUS_RESULT" == ok-sync ]]; then
    GIT_INFO+=" on $CLEAR"
    if [[ -n "$VCS_STATUS_LOCAL_BRANCH" ]]; then
      GIT_INFO+="$GREEN$VCS_STATUS_LOCAL_BRANCH"
      if [[ $VCS_STATUS_COMMITS_AHEAD -ge 1 ]]; then
        GIT_INFO+="⇡$VCS_STATUS_COMMITS_AHEAD"
      fi
      if [[ $VCS_STATUS_COMMITS_BEHIND -ge 1 ]]; then
        GIT_INFO+="⇣$VCS_STATUS_COMMITS_BEHIND"
      fi
    elif [[ $VCS_STATUS_TAG != "" ]]; then
      GIT_INFO+="$GREEN$VCS_STATUS_TAG"
    else
      GIT_INFO+="$GREEN$VCS_STATUS_COMMIT"
    fi

    GIT_INFO+="$CLEAR"

    if [[ $VCS_STATUS_HAS_STAGED -eq 1 ]] ||
       [[ $VCS_STATUS_HAS_UNSTAGED -eq 1 ]] ||
       [[ $VCS_STATUS_HAS_CONFLICTED -eq 1 ]] ||
       [[ $VCS_STATUS_HAS_UNTRACKED -eq 1 ]]; then

      if [[ $VCS_STATUS_NUM_CONFLICTED -ge 1 ]]; then
        GIT_INFO+=" $PURPLE$VCS_STATUS_ACTION~$VCS_STATUS_NUM_STAGED_NEW$CLEAR"
      fi

      if [[ $VCS_STATUS_NUM_STAGED_NEW -ge 1 ]]; then
        GIT_INFO+=" $GREEN+$VCS_STATUS_NUM_STAGED_NEW$CLEAR"
      fi

      if [[ $VCS_STATUS_NUM_STAGED_DELETED -ge 1 ]]; then
        GIT_INFO+=" $RED-$VCS_STATUS_NUM_STAGED_DELETED$CLEAR"
      fi

      if [[ $VCS_STATUS_NUM_STAGED -ge 1 ]]; then
        NUM_MODIFIED=$(($VCS_STATUS_NUM_STAGED - $VCS_STATUS_NUM_STAGED_NEW - $VCS_STATUS_NUM_STAGED_DELETED))

        if [[ $NUM_MODIFIED -gt 0 ]]; then
          GIT_INFO+=" $BLUE~$NUM_MODIFIED$CLEAR"
        fi
      fi

      (( $VCS_STATUS_HAS_UNSTAGED  )) && GIT_INFO+=" $YELLOW!$VCS_STATUS_NUM_UNSTAGED$CLEAR"
      (( $VCS_STATUS_HAS_UNTRACKED )) && GIT_INFO+=" $CYAN?$VCS_STATUS_NUM_UNTRACKED$CLEAR"
    fi
    GIT_INFO+="$CLEAR"
  fi

  PS1+=$GIT_INFO

  PROMPT_COLOR="$GREEN"
  if [[ $EXIT_CODE -ne 0 ]]; then
    PROMPT_COLOR=" $RED[$EXIT_CODE]"
  elif [[ $IN_NIX_SHELL ]]; then
    PROMPT_COLOR="$CYAN"
  fi

  PS1+="$PROMPT_COLOR\n❯$CLEAR "
}

force_color_prompt=yes
PROMPT_DIRTRIM=3

# Disable Ctrl+S behavior
stty -ixon
