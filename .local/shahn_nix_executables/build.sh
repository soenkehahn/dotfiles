#!/usr/bin/env bash

set -o errexit

echo building nix executables...

. ~/.nix-profile/etc/profile.d/nix.sh
NIX_PATH=
nice -n 19 nix-build --keep-going --max-jobs 4
