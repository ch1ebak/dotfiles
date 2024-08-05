#     ____   ___    _____  __  __  #
#    / __ ) /   |  / ___/ / / / /  #
#   / __  |/ /| |  \__ \ / /_/ /   #
#  / /_/ // ___ | ___/ // __  /    #
# /_____//_/  |_|/____//_/ /_/     #


# OPTIONS
## Exports
export TERM="alacritty"
export VISUAL="emacsclient -c -a emacs"
export EDITOR="emacsclient -t -a """
export ALTERNATE_EDITOR=""

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
PS1="\[\033[31m\]\w >\[\e[m\] "

## Fetch
fastfetch


# ALIASES
# arch - pacman and paru
alias pi="sudo pacman -S"
alias pr="sudo pacman -Rns"
alias pup="sudo pacman -Syu"
alias ps="pacman -F"
alias pc="sudo pacman -Rns $(pacman -Qtdq)"
alias pkglist="sudo pacman -Qqe > ~/Dokumenty/packages.txt"
alias yi="paru -S"
alias yr="paru -Rns"
alias yup="paru -Sua"
                                                               
## Commands
alias ..="cd .."
alias cp="cp -i"
alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs"
# alias df="duf --only local"
alias fd="fd --hidden --ignore-case"
alias grep="rg"
alias ls="ls -lA --color=always --group-directories-first"
# alias ls="eza -lA --color=always --group-directories-first"
alias mkdir="mkdir -pv"
alias mv="mv -i"
alias rm="trash -vi"
                                                               
# Backlight
alias bt="bpytop"
alias gc="git clone"
alias merge="xrdb -merge ~/.Xresources"
alias ff="fastfetch"
alias ffnc="fastfetch --config examples/6.jsonc"
alias nvt="nvtop"
alias synctime="sudo ntpd -qg && sudo hwclock -w"
                                                               
## Emacs
alias em="/usr/bin/emacs -nw"
alias emacsd="/usr/bin/emacs --daemon &"
alias kemacs="killall emacs"
                                                               
## Reboot
alias reboot="sudo reboot"
alias shutdown="sudo shutdown now"