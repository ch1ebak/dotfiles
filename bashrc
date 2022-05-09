# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# alias ls='ls --color=auto'
# PS1='[\u@\h \W]\$ '


### EYE CANDY

## Starship
eval "$(starship init bash)"

## Fetch
# pfetch
# neofetch
colorscript random

## Starship
eval "$(starship init bash)"


### ALIASES

## arch - pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -R'
alias gy='yay -S'
alias ry='yay -Rns'
alias pacsyu='sudo pacman -Syu'                  # update only standard pkgs
alias yaysyu='yay -Syu'                          # update standard pkgs and AUR pkgs (yay)
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages
alias packagelist='sudo pacman -Qe > packages.txt'

##void - xbps and xi
# alias gp='sudo xbps-install'
# alias rp='sudo xbps-remove -R'
# alias cleanup='sudo xbps-remove -Oo'
# alias xi='sudo xi'
# alias xsu='sudo xbps-install -Su'

## Bpytop
alias bt='bpytop'

## Calendar
alias cal='calcure'

## confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

## Emacs
alias em='/usr/bin/emacs -nw'
alias emacsd='/usr/bin/emacs --daemon &'
alias kemacs='killall emacs'

# Doom Emacs
alias doom='~/.emacs.d/bin/doom sync'
alias doomup='~/.emacs.d/bin/doom upgrade'
alias doomdoc='~/.emacs.d/bin/doom doctor'
alias doomrec='~/.emacs.d/bin/doom build'

# MU4E
alias mu4emu='time mu init --maildir=~/Dokumenty/org/Maildir/ --my-address=yellowparenti@disroot.org'
alias mu4emb='time mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a'

## grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

## Htop
alias ht='htop'

## Changing "ls" to "exa"
alias ls='ls -la --color=always --group-directories-first' # my preferred listing

## Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

## Mount
alias mountvm='sudo mount -t 9p -o trans=virtio /sharepoint share'

## mkdir
alias mkdir='mkdir -pv'

## Picom
alias kap='killall picom'

## Reboot
alias reboot='sudo reboot'
alias shutdown='sudo shutdown now'

## Timeshift
alias tsc='sudo timeshift --create'
alias tsl='sudo timeshift --list'

## Uptime
alias upt='uptime'

## youtube-dl
alias ydla='youtube-dl --extract-audio --audio-format best'
alias ydlv='youtube-dl -f bestvideo+bestaudio'

## Weather
alias wpl='curl wttr.in/pleszew'
alias wpo='curl wttr.in/poznan'
alias wv='curl wttr.in/irvine'
