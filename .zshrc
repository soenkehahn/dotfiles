zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'
zstyle ':completion:*' special-dirs true

fpath=(~/.zsh/completions $fpath)
autoload -U compinit
compinit -C

stty -ixon

source ~/.zshrc.aliases

# history
HISTFILE=~/.history
HISTSIZE=10000000
SAVEHIST=10000000
setopt HIST_IGNORE_SPACE HIST_IGNORE_ALL_DUPS APPEND_HISTORY
eval "$(atuin init zsh --disable-up-arrow)"

eval "$(starship init zsh)"

autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"

export EDITOR=$(which nvim)

eval "$(direnv hook zsh)"
