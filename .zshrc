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

TERM=xterm-color

# red stderr
# exec 2>>(while $IFS='' read line; do
#  print '\e[91m'${(q)line}'\e[0m' > /dev/tty; print -n $'\0'; done &)


autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000


setopt histignorespace histignoredups appendhistory listrowsfirst
unsetopt beep autocd bgnice

# vi oder emacs mode?
# bindkey -e
# End of lines configured by zsh-newuser-install


# prompt stuff

autoload -U promptinit
promptinit
autoload -U colors && colors

if [ $NIX_CUSTOM_PROFILE ] ; then
    nix_shell_prompt="[PROFILE: $NIX_CUSTOM_PROFILE]═"
else
    if [ $IN_NIX_SHELL ] ; then
        nix_shell_prompt="[[NIX-SHELL: $NIX_SHELL_EXPRESSIONS]]═"
    fi
fi

if [ $NHC_CABAL_FILE ] ; then
    nhc_cabal_file_prompt="[NHC: $NHC_CABAL_FILE]═"
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
=%(?..(%{$fg[red]$terminfo[bold]%}%?%{$reset_color%}%))=$nix_shell_prompt$nhc_cabal_file_prompt\
%(!.%{$fg[red]$terminfo[bold]%}#%{$reset_color%}.>) "

# RPROMPT (shows up at the end of a line)
# RPROMPT="\$(date)"
RPROMPT='$PROMPT_GIT_BRANCH  $PROMPT_TIME'

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi
if [ -d ~/.cabal/bin ] ; then
    PATH=~/.cabal/bin:"${PATH}"
fi
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

# python stuff
alias rmpyc='rm **/*.pyc'
alias pack='ack-grep --python '

# haskell stuff
alias hack='ack-grep --haskell '

alias -g "\&"="&>/dev/null&|"

alias o='kioclient exec '
alias am='aqualung -EN0 '
alias qmv='qmv --options=spaces '

REPORTTIME=60


function git-pull-all () {
    START=$(git branch | grep '\*' | sed 's/^.//');
    for i in $(git branch | sed 's/^.//'); do
        git checkout $i;
        git pull || break;
    done;
    git checkout $START;
};

alias gl='git log --graph --pretty=format:'\''%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --abbrev-commit --all'
alias glh='gl | head -n25'
alias qg='gitk --all \&'

alias vlch='vlc --extraintf=luahttp'

alias gpgwho='gpg --no-default-keyring --secret-keyring /dev/null -a --list-only '

alias ack=ack-grep
