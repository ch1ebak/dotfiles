#     ____   ___    _____  __  __  #
#    / __ ) /   |  / ___/ / / / /  #
#   / __  |/ /| |  \__ \ / /_/ /   #
#  / /_/ // ___ | ___/ // __  /    #
# /_____//_/  |_|/____//_/ /_/     #


# OPTIONS
## Exports
export TERM="ghostty"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND='find . \! \( -type d -path ./.git -prune \) \! -type d \! -name '\''*.tags'\'' -printf '\''%P\n'\'
export PATH="~/Projekty/scripts:~/.local/scripts:$PATH"
export MANPAGER='nvim +Man!' 

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
# minifetch
fastfetch


# ALIASES
## Package Manager
alias nsp="nix-shell -p"
alias nfu="sudo nix flake update --flake ~/.nixos-btw"
alias nurse="sudo nixos-rebuild switch --impure --flake ~/.nixos-btw#nixos-btw"
alias nconf="nvim ~/.nixos-btw/"

## Commands
### cd
alias ..="cd .."
alias cd.="cd .."
alias cd..="cd ../.."
alias cd...="cd ../../.."
alias cd....="cd ../../../.."

### directories
alias ls="ls -AGFhlv --color=always --group-directories-first"
alias mkd="mkdir -pv"

### files
alias cp="cp -i"
alias mv="mv -i"

### find
alias fd="fd -Hia --color always"
alias rg="rg -i --unrestricted --follow --pretty"

### remove
alias rm="trash -vi"
alias tr="trash -vi"

### system
alias bt="btop"
alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs --exclude-type=efivarfs"
alias reboot="sudo reboot"
alias shutdown="sudo shutdown now"
alias xbl="brightnessctl --device=intel_backlight set "

## Apps
alias cal="cal -3 -m --color=auto"
alias dunres="killall -e dunst & sleep 1; dunstify "hello!" &"
alias fehs="feh --bg-fill"
alias n="nvim" 
alias n.="nvim ." 
alias yt-dlp="yt-dlp -f bestvideo[height=1080][ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
alias y="yazi"

## Git
alias gc="git clone"

## Stow
alias stow="stow ."
alias stowa="stow . --adopt"


# FZF
eval "$(fzf --bash)"

# Zoxide
eval "$(zoxide init bash)"
