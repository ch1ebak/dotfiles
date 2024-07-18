#     ____   ___    _____  __  __  #
#    / __ ) /   |  / ___/ / / / /  #
#   / __  |/ /| |  \__ \ / /_/ /   #
#  / /_/ // ___ | ___/ // __  /    #
# /_____//_/  |_|/____//_/ /_/     #


# OPTIONS
## Exports
export TERM="alacritty"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t -a """
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
PROMPT_COMMAND="history -a"
HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
HISTTIMEFORMAT="%F %T "

## Prompt
PS1="\[\033[31m\]\w >\e[0m "
# eval "$(starship init bash)"

## Fetch
# neofetch


# ALIASES
# arch - pacman and yay
alias pi="sudo pacman -S"
alias pr="sudo pacman -Rns"
alias pup="sudo pacman -Syu"
alias ps="pacman -F"
alias pc="sudo pacman -Rns $(pacman -Qtdq)"
alias pkglist="sudo pacman -Qqe > ~/Dokumenty/packages.txt"
alias pkgcount="pacman -Q | wc -l"
alias yi="paru -S"
alias yr="paru -Rns"
alias yup="paru -Sua"

## Commands
alias ..="cd .."
alias cp="cp -i"
alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs"
alias fd="fd --hidden --ignore-case"
alias grep="rg"
alias ls="ls -lA --color=always --group-directories-first"
alias mkdir="mkdir -pv"
alias mv="mv -i"
alias rm="trash -vi"
                                                               
# Backlight
alias bt="bpytop"
alias gc="git clone"
alias merge="xrdb -merge ~/.Xresources"
alias nfnc="neofetch --config none"
alias nvt="nvtop"
alias picom="picom -b"
alias synctime="sudo ntpd -qg && sudo hwclock -w"
alias xbl="xbacklight -set "
                                                               
## Emacs
alias em="/usr/bin/emacs -nw"
alias emacsd="/usr/bin/emacs --daemon &"
alias kemacs="killall emacs"
                                                               
## Reboot
alias reboot="sudo reboot"
alias shutdown="sudo shutdown now"
