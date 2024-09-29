#!/bin/sh

shuf -e -n1 $HOME/Obrazy/tapety/* | xargs feh --bg-fill
