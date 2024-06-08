if [[ -z "${SSH_AGENT_PID}" ]]; then
  eval $(ssh-agent)
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

pathadd() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    PATH="$1${PATH:+":$PATH"}"
  fi
}

for dir in /usr/local/go/bin /nix/var/nix/profiles/default/bin; do
  pathadd "$dir"
done

for dir in .local/bin .cabal/bin .cargo/bin .ghcup/bin go/bin .nix-profile/bin; do
  pathadd "$HOME/$dir"
done

export BROWSER=~/.nix-profile/bin/firefox

# For nix
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

export GDK_DPI_SCALE=2
export QT_SCALE_FACTOR=2
