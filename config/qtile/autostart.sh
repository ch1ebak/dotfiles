#!/usr/bin/env bash

~/.config/arandr/default.sh &
udiskie -a &
/usr/bin/emacs --daemon &
nitrogen --restore &
picom --config  $HOME/.config/picom/picom.conf &
signal-desktop &
