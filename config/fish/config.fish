###################################
### https://github.com/ch1ebak/ ###
###################################


### EYE CANDY
# fish_config theme choose catppuccin
# fish_config theme choose dracula
# fish_config theme choose gruvbox
# fish_config theme choose nord
fish_config theme choose spacegray

## Fetch
# colorscript random
# starfetch
# treefetch
neofetch
# fetchit

## Starship
starship init fish | source


### OPTIONS

if status is-interactive
    # Commands to run in interactive sessions can go here
end

## Path
export PATH="$PATH:$HOME/.config/fish/config.fish"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="/usr/bin/sxiv:$PATH"
set -e fish_user_paths
set -U fish_user_paths $HOME/.local/bin $HOME/Applications $fish_user_paths

## Exports
set -g fish_greeting
set TERM "kitty"
set EDITOR "emacsclient -t -a ''"
set VISUAL "emacsclient -c -a emacs"
set BROWSER "firefox"

## Keybindings
function fish_user_key_bindings
  fish_vi_key_bindings
end

## Functions needed for !! and !$
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
# The bindings for !! and !$
if [ $fish_key_bindings = "fish_vi_key_bindings" ];
  bind -Minsert ! __history_previous_command
  bind -Minsert '$' __history_previous_command_arguments
else
  bind ! __history_previous_command
  bind '$' __history_previous_command_arguments
end


### ALIASES

## package manager
# arch - pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -Rns'
alias pacup='sudo pacman -Syu'
alias pacs='pacman -F'
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/packages.txt'
alias pkgcount='pacman -Q | wc -l'
alias gy='yay -S'
alias ry='yay -Rns'
alias yayup='yay -Sua'
alias update='yay -Syu'

# void - xbps
# alias gp='sudo xbps-install'
# alias rp='sudo xbps-remove -R'
# alias xup='sudo xbps-install -Su'
# alias cleanup='sudo xbps-remove -Oo'
# alias pacs='xbps-query -R'
# alias pacerror='xbps-pkgdb -a'

# Commands
alias cat="bat"
alias cp="cp -i"
alias df="duf"
# alias ls='ls -la --color=always --group-directories-first'
alias ls='exa -la --color=always --group-directories-first'
alias mkdir='mkdir -pv'
alias mv='mv -i'
# alias rm='rm -i'
alias rm='trash -vi'

# Bpytop
alias bt='bpytop'

# Dunst
alias dunstres='killall dunst && dunst & && dunstify "hewwo"'

# Emacs
alias em='/usr/bin/emacs -nw'
alias emacsd='/usr/bin/emacs --daemon &'
alias kemacs='killall emacs'

# Doom Emacs
alias dooms='~/.emacs.d/bin/doom sync'
alias doomup='~/.emacs.d/bin/doom upgrade'
alias doomdoc='~/.emacs.d/bin/doom doctor'
alias doomrec='~/.emacs.d/bin/doom build'
alias doomref='~/.emacs.d/bin/doom refresh'

# MU4E
alias mu4emu='time mu init --maildir=~/Dokumenty/Maildir/'
alias mu4emb='time mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a'

# Git
alias gtc='git clone'
alias dots='git clone https://github.com/ch1ebak/dotfiles'
alias stpage='git clone https://github.com/ch1ebak/ch1ebak.github.io'

# grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

# Music
alias mixer='ncpamixer'
# alias music='mpd ~/.config/mpd/mpd.conf && yams -p 6600 -r && ncmpcpp'
alias music='mpd ~/.config/mpd/mpd.conf && mpdscribble -D --conf ~/.mpdscribble/mpdscribble.conf &'
alias mpdd='mpd ~/.config/mpd/mpd.conf &'
alias mpds='mpdscribble -D --conf ~/.mpdscribble/mpdscribble.conf &'
alias ncpc='ncmpcpp'

# Picom
alias kap='killall picom'
alias picom='picom --experimental-backend -b'

# Reboot
alias reboot='sudo reboot'
alias shutdown='sudo shutdown now'

# Sync time
alias synctime='sudo ntpd -qg && sudo hwclock -w'

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

# xinput
alias kbin='xinput reattach 20 3'
alias kbout='xinput float 20'
