###################################
### https://github.com/ch1ebak/ ###
###################################


### Options

## Vim keybindings
set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

## Options
complete -cf sudo


### EYE CANDY

## Starship
eval "$(starship init bash)"

## Fetch
# colorscript random
# starfetch
# treefetch
neofetch
# fetchit
