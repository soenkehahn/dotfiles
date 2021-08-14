#!/usr/bin/env bash

set -o errexit

echo building nix executables...

. ~/.nix-profile/etc/profile.d/nix.sh
NIX_PATH=
cd $(dirname $0)
nice -n 19 nix-build
