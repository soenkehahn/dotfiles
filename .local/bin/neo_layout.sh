#!/bin/bash

set -o errexit

setxkbmap lv
xmodmap ~/.local/neo_de.xmodmap
xset -r 51
