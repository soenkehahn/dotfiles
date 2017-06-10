#!/bin/bash

set -o errexit

setxkbmap lv
xmodmap ~/.local/neo_de.$(hostname).xmodmap
xset -r 51
