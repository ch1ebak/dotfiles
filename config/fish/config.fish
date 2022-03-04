if status is-interactive
    # Commands to run in interactive sessions can go here
end

### PATH
# ~/.config/fish/config.fish
export PATH="$HOME/.emacs.d/bin:$PATH"
set -e fish_user_paths
set -U fish_user_paths $HOME/.local/bin $HOME/Applications $fish_user_paths


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

## Adblock
alias adblock='doas /usr/local/bin/hblock'

## Ani-cli
alias anime='ani-cli -vq best'

## Bpytop
alias bt='bpytop'

## Calendar
alias cal='calcure'

## Cds
alias .1='cd ..'
alias .2='cd ../..'
alias .3='cd ../../..'

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

## Corona
alias covidpl='corona poland'

## Emacs
alias emacs='emacsclient -c -a 'emacs''
alias em='emacsclient -nw'
alias emacsd='/usr/bin/emacs --daemon &'
alias doom='~/.emacs.d/bin/doom sync'
alias kemacs='killall emacs'

## grep
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

## grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

## soccer-cli
alias mcfc='doas soccer --team=MCFC --upcoming'

## Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

## Mixer
alias mixer='ncpamixer'

## mirrors
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

## mkdir
alias mkdir='mkdir -pv'

## Monitors
alias mboth='fish ~/.screenlayout/benq-dell.sh'

## ncmpcpp
alias music='ncmpcpp'

## neovim
alias vim='nvim'

## pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -R'
alias gy='yay -S'
alias ry='yay -Rns'
alias pacsyu='sudo pacman -Syu'                 # update only standard pkgs
alias yaysua='yay -Sua'              # update only AUR pkgs (yay)
alias yaysyu='yay -Syu'              # update standard pkgs and AUR pkgs (yay)
alias parsua='paru -Sua'             # update only AUR pkgs (paru)
alias parsyu='paru -Syu'             # update standard pkgs and AUR pkgs (paru)
alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages
alias packagelist='sudo pacman -Qe > packages.txt'

## Ratesx
alias ratesx='curl rate.sx'

## Root priviledges
alias sudo='doas'

## Todoist
# alias tdl='todoist list'
# alias tda='todoist add'
# alias tdc='todoist close'
# alias tdd='todoist delete'
# alias tds='todoist sync'

## Terminal
alias :q='exit'

## youtube-dl
# alias yta-aac="youtube-dl --extract-audio --audio-format aac "
# alias yta-best="youtube-dl --extract-audio --audio-format best "
# alias yta-flac="youtube-dl --extract-audio --audio-format flac "
# alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
# alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
# alias yta-opus="youtube-dl --extract-audio --audio-format opus "
# alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
# alias yta-wav="youtube-dl --extract-audio --audio-format wav "
# alias ytv-best="youtube-dl -f bestvideo+bestaudio "

## Weather
alias wpl='curl wttr.in/pleszew'
alias wpo='curl wttr.in/poznan'
alias wv='curl wttr.in/irvine'
export PATH="$PATH:$HOME/.spicetify"
