source ~/.zshrc.old

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'
zstyle ':completion:*' special-dirs true

source ~/.zshrc.aliases

# history
HISTFILE=~/.history
HISTSIZE=10000000
SAVEHIST=10000000
setopt HIST_IGNORE_SPACE HIST_IGNORE_ALL_DUPS APPEND_HISTORY

source /usr/share/doc/fzf/examples/key-bindings.zsh

eval "$(starship init zsh)"
