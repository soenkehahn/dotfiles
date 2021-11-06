# if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

for dir in .local/shahn_nix_executables/result/bin .local/bin .cargo/bin .ghcup/bin; do
  if [[ -d "$HOME/$dir" ]]; then
    PATH="$HOME/$dir:$PATH"
  fi
done
