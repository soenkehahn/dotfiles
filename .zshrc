source ~/.zshrc.old

source ~/.zshrc.aliases

# history
HISTFILE=~/.history
HISTSIZE=10000000
SAVEHIST=10000000
setopt HIST_IGNORE_SPACE HIST_IGNORE_ALL_DUPS APPEND_HISTORY

source /usr/share/doc/fzf/examples/key-bindings.zsh

eval "$(starship init zsh)"
