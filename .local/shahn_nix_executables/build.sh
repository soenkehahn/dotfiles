#!/usr/bin/env bash

set -o errexit

echo building nix executables...

if [ -f "~/.nix-profile/etc/profile.d/nix.sh" ]; then
  . ~/.nix-profile/etc/profile.d/nix.sh
fi
NIX_PATH=
cd $(dirname $0)
nice -n 19 nix-build
