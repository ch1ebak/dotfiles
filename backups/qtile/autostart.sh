#!/usr/bin/env bash 

xrandr --output HDMI-1-0 --primary --mode 1920x1080 --pos 1920x0 --rate 144 --output eDP-2 --mode 1920x1080 --pos 0x0 --rate 165
brightnessctl --device=intel_backlight set 40%
dunst &
nm-applet &
easyeffects --gapplication-service &
emacs --daemon &
