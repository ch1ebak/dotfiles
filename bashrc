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
export MANPAGER="less"
export FZF_DEFAULT_COMMAND='find . \! \( -type d -path ./.git -prune \) \! -type d \! -name '\''*.tags'\'' -printf '\''%P\n'\'

## Options
bind "set colored-stats on"
bind "set colored-completion-prefix on"
bind "set completion-ignore-case on"
bind "set completion-map-case on"
bind "set show-all-if-ambiguous on"
bind "set mark-symlinked-directories on"
complete -cf sudo

## Keybindings
set -o vi
bind Space:magic-space
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

## Shopt
shopt -s autocd
shopt -s cdspell
shopt -s histappend
shopt -s cmdhist

## History
export PROMPT_COMMAND="history -a"
export HISTSIZE=2000
export HISTFILESIZE=2000
export HISTCONTROL=erasedups:ignoredups:ignorespace

## Prompt
PS1="\[\033[31m\]\w >\[\e[m\] "

## Fetch
fastfetch


# ALIASES
## arch - pacman and paru
alias pi="paru -S"
alias pr="paru -Rns"
alias pup="paru -Syu"
alias pkglist="paru -Qqe > ~/Dokumenty/packages.txt"
alias pri="paru -Q | rg"
alias prs="pacman -Ss"
                                                               
## Commands
alias ..="z .."
alias z.="z .."
alias cd="z"
alias cp="cp -i"
alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs"
alias fd="fd --hidden --ignore-case"
alias grep="rg -i --hidden"
alias ls="ls -lA --color=always --group-directories-first"
alias mkd="mkdir -pv"
alias mv="mv -i"
alias rm="trash -vi"
                                                               
## Reboot
alias reboot="sudo reboot"
alias shutdown="sudo shutdown now"
                                                               
## Apps
alias bt="bpytop"
alias batsig="batsignal -w 20 -c 15 -d 5 -p -f 90 -b"
alias dunres="killall -e dunst & sleep 1; dunstify "hello!" &"
alias ff="fastfetch"
alias nvt="nvtop"
alias yt-dlp="yt-dlp -f bestvideo[height=1080][ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"

## Git
alias gc="git clone"
alias gdots="git clone https://github.com/ch1ebak/dotfiles"
                                                               
## Emacs
alias em="/usr/bin/emacs -nw"
alias emacsd="/usr/bin/emacs --daemon &"
alias kemacs="killall emacs"

## System
alias merge="xrdb -merge ~/.Xresources"
alias xbl="xbacklight -set"

## Wallpapers
alias fehs="feh --bg-fill"
alias wlp="shuf -e -n1 $HOME/Obrazy/tapety/* | xargs feh --bg-fill"

# FZF
eval "$(fzf --bash)"

# Zoxide
eval "$(zoxide init bash)"