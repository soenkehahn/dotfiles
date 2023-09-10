if [[ -z "${SSH_AGENT_PID}" ]]; then
  eval $(ssh-agent)
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

for dir in .local/bin .cabal/bin .cargo/bin .ghcup/bin go/bin .nix-profile/bin; do
  if [[ -d "$HOME/$dir" ]]; then
    PATH="$HOME/$dir:$PATH"
  fi
done

for dir in /usr/local/go/bin /nix/var/nix/profiles/default/bin; do
  if [[ -d "$dir" ]]; then
    PATH="$dir:$PATH"
  fi
done

# For nix
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

export GDK_DPI_SCALE=2
export QT_SCALE_FACTOR=2
