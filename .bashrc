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
## Package Manager
alias pi="paru -S"
alias pr="paru -Rns"
alias pup="paru -Syu"
alias pkglist="paru -Qqe > ~/Dokumenty/packages.txt"
alias pq="paru -Q | rg"
alias ps="pacman -Ss"
alias pp="paru -Ss"
                                                               
## Commands
### cd
alias cd="z"
alias ..="z .."
alias z.="z .."
alias z..="z ../.."
alias z...="z ../../.."
alias z....="z ../../../.."

### directories
alias ls="ls -AGFhlv --color=always --group-directories-first"
alias mkd="mkdir -pv"

### files
alias cp="cp -i"
alias mv="mv -i"

### find
alias fd="find . -type f -name"
alias rg="rg -i --hidden"

### remove
alias rm="trash -vi"
alias tr="trash -vi"

### system
alias bt="btop"
alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs"
alias ff="fastfetch"
alias ffnc="fastfetch --config examples/6.jsonc"
alias merge="xrdb -merge ~/.Xresources"
alias nvt="nvtop"
alias reboot="sudo reboot"
alias shutdown="sudo shutdown now"
alias xbl="brightnessctl set "

## Apps
alias dunres="killall -e dunst & sleep 1; dunstify "hello!" &"
alias fehs="feh --bg-fill"
alias lg="lazygit"
alias yz="yazi"
alias yt-dlp="yt-dlp -f bestvideo[height=1080][ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"

## Git
alias gc="git clone"
alias gdots="git clone https://github.com/ch1ebak/dotfiles"

## Neovim
alias nv="nvim"
alias vim="nvim"

## Stow
alias stow="stow ."
alias stowa="stow . --adopt"


# FZF
eval "$(fzf --bash)"

# Zoxide
eval "$(zoxide init bash)"
