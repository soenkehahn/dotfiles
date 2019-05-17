#!/usr/bin/env bash

echo building nix executables...

. ~/.nix-profile/etc/profile.d/nix.sh
NIX_PATH=
nice -n 19 nix-build -j4
