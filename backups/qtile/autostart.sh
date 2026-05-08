#!/usr/bin/env bash 

xrandr --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rate 144 --output DP-0 --mode 1920x1080 --pos 0x0 --rate 165
nm-applet &
picom -b
dunst &
brightnessctl set 50%
easyeffects --gapplication-service &
/usr/bin/emacs --daemon &
