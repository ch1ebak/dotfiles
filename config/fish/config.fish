#     ______ ____ _____  __  __   _____  __  __ ______ __     __  #
#    / ____//  _// ___/ / / / /  / ___/ / / / // ____// /    / /  #
#   / /_    / /  \__ \ / /_/ /   \__ \ / /_/ // __/  / /    / /   #
#  / __/  _/ /  ___/ // __  /   ___/ // __  // /___ / /___ / /___ #
# /_/    /___/ /____//_/ /_/   /____//_/ /_//_____//_____//_____/ #


# OPTIONS
if status is-interactive
    # Commands to run in interactive sessions can go here
end

## Path
set -e fish_user_paths
set -U fish_user_paths $HOME/.bin  $HOME/.local/bin $HOME/.config/emacs/bin $HOME/Applications /usr/bin/nsxiv $HOME/.config/rofi/modules $fish_user_paths

## Exports
set -g fish_greeting
set -g FZF_CTRL_T_COMMAND "command find -L \$dir -type f 2> /dev/null | sed '1d; s#^\./##'"
set TERM "alacritty"
set EDITOR "emacsclient -t -a ''"
set VISUAL "emacsclient -c -a emacs"
set BROWSER "firefox"
set -x MANPAGER "less"


# Keybindings
## Vi mode
function fish_user_key_bindings
  fish_vi_key_bindings
end

## !!
### Functions needed for !! and !$
function __history_previous_command
  switch (commandline -t)
  case "!"
    commandline -t $history[1]; commandline -f repaint
  case "*"
    commandline -i !
  end
end
function __history_previous_command_arguments
  switch (commandline -t)
  case "!"
    commandline -t ""
    commandline -f history-token-search-backward
  case "*"
    commandline -i '$'
  end
end

### The bindings for !! and !$
if [ $fish_key_bindings = "fish_vi_key_bindings" ];
  bind -Minsert ! __history_previous_command
  bind -Minsert '$' __history_previous_command_arguments
else
  bind ! __history_previous_command
  bind '$' __history_previous_command_arguments
end


# EYE CANDY
## Themes
# fish_config theme choose dracula
fish_config theme choose spacegray

## Fetch
neofetch

## Prompt
starship init fish | source


# ALIASES
# arch - pacman and paru
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
#alias df="df -hl --exclude-type=tmpfs --exclude-type=devtmpfs"
alias df="duf --only local"
alias fd="fd --hidden --ignore-case"
alias grep="rg"
#alias ls="ls -lA --color=always --group-directories-first"
alias ls="eza -lA --color=always --group-directories-first"
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
