if [[ -z "${SSH_AGENT_PID}" ]]; then
  eval $(ssh-agent)
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

for dir in .local/bin .cabal/bin .cargo/bin .ghcup/bin go/bin; do
  if [[ -d "$HOME/$dir" ]]; then
    PATH="$HOME/$dir:$PATH"
  fi
done

if [[ -d /usr/local/go/bin ]]; then
  PATH="/usr/local/go/bin:$PATH"
fi

# For nix
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

export GDK_DPI_SCALE=2
export QT_SCALE_FACTOR=2
