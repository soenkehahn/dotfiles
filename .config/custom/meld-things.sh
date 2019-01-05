#!/usr/bin/env bash

sudo meld ~/.config/custom/evdev /usr/share/X11/xkb/keycodes/evdev
sudo meld ~/.config/custom/neo-de /usr/share/X11/xkb/symbols/de

diff <(gpg --decrypt ~/.config/custom/sudoers.gpg) <(sudo cat /etc/sudoers) || \
  echo 'Please, use visudo!'

