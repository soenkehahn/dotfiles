# The following lines were added by compinstall

zstyle ':completion:*' completer _complete
zstyle ':completion:*' completions unset
zstyle ':completion:*' file-sort name
zstyle ':completion:*' glob unset
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'm:{a-z}={A-Z}' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=long
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt 'da hast du %e fehler gemacht'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true
zstyle :compinstall filename '/home/shahn/.zshrc'



bindkey -e

bindkey "^[[3~" delete-char
bindkey "^[3;5~" delete-char
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line

TERM=xterm-256color

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000

setopt histignorespace histignoredups appendhistory listrowsfirst
unsetopt beep autocd bgnice

# prompt stuff

autoload -U promptinit
promptinit
autoload -U colors && colors

if [ -z "$CABAL_SANDBOX_CONFIG" ]; then
  cabal_sandbox_hint=""
else
  cabal_sandbox_hint="sandbox: $(basename $(dirname $CABAL_SANDBOX_CONFIG))="
fi

setopt prompt_subst
precmd () {
  PROMPT_TIME=$(date +%H:%M:%S)
  PROMPT_GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
  PROMPT_GIT_BRANCH="$PROMPT_GIT_BRANCH $(git rev-parse --short HEAD 2> /dev/null)"
}

# PROMPT explanation:
# %n: username, %m: hostname
# %(!.#.$): if (su_priv) then # else $
PROMPT="\
%{$terminfo[bold]%}%n%{$reset_color%}@\
%{$fg[blue]$terminfo[bold]%}%m%{$reset_color%}:\
%{$fg[red]$terminfo[bold]%}%~%{$reset_color%}
=%(?..(%{$fg[red]$terminfo[bold]%}%?%{$reset_color%}%))=$cabal_sandbox_hint\
%(!.%{$fg[red]$terminfo[bold]%}#%{$reset_color%}.>) "

# RPROMPT (shows up at the end of a line)
# RPROMPT="\$(date)"
RPROMPT='$PROMPT_GIT_BRANCH  $PROMPT_TIME'

# set PATH so it includes user's private bin if it exists
if [ -d ~/local/bin ] ; then
    PATH=~/local/bin:"${PATH}"
fi


# make / a word separator
local WORDCHARS=${WORDCHARS//\//}

# color stderr
# exec 2>>(while read line; do
#   print '\e[91m'${(q)line}'\e[0m' > /dev/tty; done &)

export EDITOR=$(which vim)

export DARCS_ALWAYS_COLOR=1
export DARCS_DO_COLOR_LINES=1

export GPG_TTY=$(tty)

###########
# ALIASES #
###########

# ls colors
alias ls="ls --color -F"
alias l="ls -lh"
alias ll="ls -l"
alias la="ls -lha"
alias rtl="ls -rtlh"

# deb stuff
alias debhavei="dpkg -l | grep -i "
alias debinstall="sudo apt-get install "
alias debsearch='apt-cache search '
alias debfilessearch='apt-file search '
alias debremove='sudo apt-get remove '
alias debclean='wajig remove-orphans && sudo apt-get autoclean && sudo apt-get clean && sudo apt-get autoremove'
alias debdailyupgrade='sudo apt-get update && sudo apt-get dist-upgrade'
alias debupdate='sudo apt-get update'
alias debupgrade='sudo apt-get upgrade'
alias debdistupgrade='sudo apt-get dist-upgrade'
alias debdetails='apt-cache policy '
alias debdepends='apt-cache depends '
alias debrdepends='apt-cache rdepends '
alias debpurge='sudo apt-get purge '

alias dusch="du -sch *"
alias isrunning='echo "  PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND" ; top -bn1 | grep '

alias -g "\&"="&>/dev/null&|"

alias am='aqualung -EN0 '
alias qmv='qmv --options=spaces '

REPORTTIME=60

alias glh='gl | head -n25'
alias qg='gitk --all \&'

alias gpgwho='gpg --no-default-keyring --secret-keyring /dev/null -a --list-only '

alias ack=ack-grep
alias hack='ack-grep --haskell '
