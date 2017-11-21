#!/bin/bash

set -o errexit

echo switching to neo...
sleep 1

scriptDir=$(dirname $0)
localDir="$scriptDir/.."

setxkbmap lv
xmodmap "$localDir/neo_de.$(hostname).xmodmap"
xset -r 51
