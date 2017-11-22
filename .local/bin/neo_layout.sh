#!/bin/bash

set -o errexit

echo switching to neo...
sleep 1

scriptDir=$(dirname $0)
localDir="$scriptDir/.."

if [[ -z "${HOSTNAME}" ]]; then
  HOSTNAME="$(hostname)"
fi

setxkbmap lv
xmodmap "$localDir/neo_de.$HOSTNAME.xmodmap"
xset -r 51
