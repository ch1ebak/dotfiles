if status is-interactive
    # Commands to run in interactive sessions can go here
end

### PATH
# ~/.config/fish/config.fish
export PATH="$HOME/.emacs.d/bin:$PATH"
set -e fish_user_paths
set -U fish_user_paths $HOME/.local/bin $HOME/Applications $fish_user_paths
export PATH="$PATH:$HOME/.spicetify"
fish_add_path /home/kd/.spicetify

### EXPORTS
set -g fish_greeting
set TERM "kitty"
set EDITOR "emacsclient -t -a ''"
set VISUAL "emacsclient -c -a emacs"
set BROWSER "firefox"


### EYE CANDY

## Color themes
# set -Ux FZF_DEFAULT_OPTS "--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4"
# set theme_color_scheme dracula
# set theme_color_scheme nord
# set theme_color_scheme gruvbox
set theme_color_scheme catppuccin

## Fetch
# pfetch
# rxfetch
# treefetch
# figlet gay rights | gay -l
colorscript random

# Starship
starship init fish | source


### ALIASES

## Bpytop
alias bt='bpytop'

## Calendar
alias cal='calcure'

## Changing "ls" to "exa"
alias ls='exa -al --color=always --group-directories-first' # my preferred listing
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'

## confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

## Emacs
alias emacsd='/usr/bin/emacs --daemon &'
alias doom='~/.emacs.d/bin/doom sync'
alias doomup='~/.emacs.d/bin/doom upgrade'
alias kemacs='killall emacs'

## grep
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

## grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

## Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

## mkdir
alias mkdir='mkdir -pv'

## pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -R'
alias gy='yay -S'
alias ry='yay -Rns'
alias pacsyu='sudo pacman -Syu'                 # update only standard pkgs
alias yaysyu='yay -Syu'              # update standard pkgs and AUR pkgs (yay)
alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages
alias packagelist='sudo pacman -Qe > packages.txt'

## Root priviledges
alias sudo='doas'

## Terminal
alias :q='exit'

## Weather
alias wpl='curl wttr.in/pleszew'
alias wpo='curl wttr.in/poznan'
alias wv='curl wttr.in/irvine'
