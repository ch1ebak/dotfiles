#!/usr/bin/bash

xrandr --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --rate 144 --output DP-0 --mode 1920x1080 --pos 0x0 --rotate normal --rate 165
/usr/bin/emacs --daemon &
nm-applet &
picom -b
dunst &
brightnessctl set 60%
shuf -e -n1 $HOME/Obrazy/tapety/sg/* | xargs feh --bg-fill
