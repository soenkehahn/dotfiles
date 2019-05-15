#!/usr/bin/env bash

echo building nix executables...

. ~/.nix-profile/etc/profile.d/nix.sh
NIX_PATH=
nix-build -j4
