###################################
### https://github.com/ch1ebak/ ###
###################################


### OPTIONS

## Exports
export TERM="alacritty"                           # getting proper colors
export ALTERNATE_EDITOR=""                        # setting for emacsclient
export EDITOR="emacsclient -t -a ''"              # $EDITOR use Emacs in terminal
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode

## Paths
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.emacs.d/bin/" ] ;
  then PATH="$HOME/.emacs.d/bin/:$PATH"
fi

## Vim keybindings
set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

## Options
complete -cf sudo
bind Space:magic-space
CDPATH="."

bind "set completion-ignore-case on"
bind "set completion-map-case on"
bind "set show-all-if-ambiguous on"
bind "set mark-symlinked-directories on"

shopt -s histappend
shopt -s cmdhist
PROMPT_COMMAND='history -a'
HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
HISTTIMEFORMAT='%F %T '


### ALIASES

## package manager
# arch - pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -Rns'
alias pacup='sudo pacman -Syu'
alias pacs='pacman -F'
alias cleanup='sudo pacman -Qtdq | sudo pacman -Rns -'
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/packages.txt'
alias pkgcount='pacman -Q | wc -l'
alias gy='yay -S'
alias ry='yay -Rns'
alias yayup='yay -Sua'
alias allup='yay -Syu'

# void - xbps
# alias gp='sudo xbps-install'
# alias rp='sudo xbps-remove -R'
# alias xup='sudo xbps-install -Su'
# alias cleanup='sudo xbps-remove -Oo'
# alias pacs='xbps-query -R'
# alias pacerror='xbps-pkgdb -a'

# Commands
alias cp="cp -i"
alias df='df -h'
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'
alias ls='ls -la --color=always --group-directories-first'
alias merge='xrdb -merge ~/.Xresources'
alias mkdir='mkdir -pv'
alias mv='mv -i'
alias reboot='sudo reboot'
# alias rm='rm -i'
alias rm='trash -vi'
alias shutdown='sudo shutdown now'

# Programs
alias bt='bpytop'
alias dunstres='killall dunst && dunst & && dunstify "hewwo"'
alias picom='picom --experimental-backend -b'

# Emacs
alias dooms='~/.emacs.d/bin/doom sync'
alias emacsd='/usr/bin/emacs --daemon &'
alias kemacs='killall emacs'
alias bal='ledger --empty -S -T -f ~/Dokumenty/ledger/ledger.dat bal ^assets:Santander assets:gotówka assets:drobne assets:SteamWallet assets:PayPal'
alias mu4emu='time mu init --maildir=~/Dokumenty/Maildir/'
alias mu4emb='time mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a'

# Git
alias gtc='git clone'
alias dots='git clone https://github.com/ch1ebak/dotfiles'
alias stpage='git clone https://github.com/ch1ebak/ch1ebak.github.io'

# Music
alias mixer='ncpamixer'
# alias music='mpd ~/.config/mpd/mpd.conf && yams -p 6600 -r && ncmpcpp'
alias music='mpd ~/.config/mpd/mpd.conf && mpdscribble -D --conf ~/.mpdscribble/mpdscribble.conf &'
alias mpdd='mpd ~/.config/mpd/mpd.conf &'
alias mpds='mpdscribble -D --conf ~/.mpdscribble/mpdscribble.conf &'
alias ncpc='ncmpcpp'

# Timeshift
alias tsc='sudo timeshift --create'
alias tsl='sudo timeshift --list'
alias tgui='sudo timeshift-gtk'

# Vim
alias vfzf='vim $(fzf)'
alias vconf='vim ~/.vimrc'

# yt-dlp
alias yt='yt-dlp -x --audio-format flac --audio-quality 0'
alias ytv='yt-dlp -f mp4'


### EYE CANDY

## Starship
eval "$(starship init bash)"

## Fetch
# neofetch
# pfetch
