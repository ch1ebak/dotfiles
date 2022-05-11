# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

### Options

## Path
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/Applications" ] ;
  then PATH="$HOME/Applications:$PATH"
fi

## Vi mode
set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'


### EYE CANDY

## Starship
eval "$(starship init bash)"

## Fetch
# colorscript random
# neofetch
# pfetch
# ppfetch
starfetch

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
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/kodowanie/packages.txt'

##void - xbps and xi
# alias gp='sudo xbps-install'
# alias rp='sudo xbps-remove -R'
# alias cleanup='sudo xbps-remove -Oo'
# alias xi='sudo xi'
# alias xsu='sudo xbps-install -Su'

## Bpytop
alias bt='bpytop'

## confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

## Curl
alias ccs='curl cheat.sh'
alias cip='curl ifconfig.co'
alias cnws='curl pl.getnews.tech'
alias cbtc='curl rate.sx'
alias cwpl='curl wttr.in/pleszew'
alias cwpo='curl wttr.in/poznan'
alias cwv='curl wttr.in/irvine'

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

## Changing "ls" to "exa"
alias ls='ls -la --color=always --group-directories-first' # my preferred listing

## Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

## Mixer
alias mixer='ncpamixer'

## Mount
alias mountvm='sudo mount -t 9p -o trans=virtio /sharepoint share'

## mkdir
alias mkdir='mkdir -pv'

## Neovim
alias vim='nvim'
alias neovim='nvim'

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
