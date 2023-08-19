###################################
### https://github.com/ch1ebak/ ###
###################################


### OPTIONS

## Exports
export TERM="alacritty"
export ALTERNATE_EDITOR=""
# export EDITOR="emacsclient -t -a ''"
export EDITOR="nvim"
export VISUAL="emacsclient -c -a emacs"

## Paths
export PATH="$HOME/.config/emacs/bin:$PATH"
export TERM=xterm-256color

## Options
set -o vi
set colored-stats on
complete -cf sudo
bind Space:magic-space
CDPATH="."
bind "set completion-ignore-case on"
bind "set completion-map-case on"
bind "set show-all-if-ambiguous on"
bind "set mark-symlinked-directories on"
shopt -s cdspell
shopt -s histappend
shopt -s cmdhist
PROMPT_COMMAND='history -a'
HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
HISTTIMEFORMAT='%F %T '

## Prompt
PS1="\w > "


### ALIASES
# arch - pacman and yay
alias pi='sudo pacman -S'
alias pr='sudo pacman -Rns'
alias pup='sudo pacman -Syu'
alias ps='pacman -F'
alias pc='sudo pacman -Rns $(pacman -Qtdq)'
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/packages.txt'
alias pkgcount='pacman -Q | wc -l'
alias yi='paru -S'
alias yr='paru -Rns'
alias yup='paru -Sua'
alias update='paru -Syu'

# Commands
alias cp="cp -i"
alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs"
alias fd="fd --hidden --ignore-case"
alias grep="rg"
alias ls='ls -lA --color=always --group-directories-first'
alias mkdir='mkdir -pv'
alias mv='mv -i'
alias rm='trash -vi'

# Bpytop
alias bt='bpytop'

# calcure
alias cal='calcure'

# Git
alias gtc='git clone'
alias dots='git clone https://github.com/ch1ebak/dotfiles'

# grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

# Music
alias music='mpd ; mpdscribble --conf ~/.config/mpdscribble/mpdscribble.conf'
alias ncpc='ncmpcpp'

# Neofetch
alias nf="neofetch"
alias nfnc="neofetch --config none"

# Picom
# alias picom='picom --experimental-backend -b'
alias picom='picom -b'

# Reboot
alias reboot='sudo reboot'
alias shutdown='sudo shutdown now'

# Sync time
alias synctime='sudo ntpd -qg && sudo hwclock -w'

# yt-dlp
alias yt='yt-dlp -x --audio-format flac --audio-quality 0'
alias ytv='yt-dlp -f mp4'

# Vim
alias nv='nvim'
alias nvim='nvim /home/kd/Projekty/nvim-start/nvim-start.md'
alias nvc='cd ~/.config/nvim'

# xinput
alias kbin='xinput reattach 20 3'
alias kbout='xinput float 20'
alias skpl='setxkbmap -layout pl'
