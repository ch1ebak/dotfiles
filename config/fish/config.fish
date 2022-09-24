###################################
### https://github.com/ch1ebak/ ###
###################################


if status is-interactive
    # Commands to run in interactive sessions can go here
end

### OPTIONS

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


### EYE CANDY
# fish_config theme choose catppuccin
# fish_config theme choose dracula
# fish_config theme choose everforest
fish_config theme choose gruvbox
# fish_config theme choose nord
# fish_config theme choose one-dark
# fish_config theme choose solarized-dark
# fish_config theme choose tokyo-night


## Fetch
# colorscript random
# starfetch
# treefetch
neofetch
# fetchit

## Starship
# starship init fish | source


### ALIASES

# arch - pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -R'
alias pacup='sudo pacman -Syu'
alias pacs='pacman -F'
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/packages.txt'
alias pkgcount='pacman -Q | wc -l'
alias gy='yay'
alias ry='yay -R'
alias yayup='yay -Syu'

# Bpytop
alias bt='bpytop'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# Curl
alias wpl='curl wttr.in/pleszew'
alias wpo='curl wttr.in/poznan'
alias wca='curl wttr.in/irvine'

# Dunst
alias dunstres='killall dunst && dunst & && dunstify "hewwo"'

# Emacs
alias em='/usr/bin/emacs -nw'
alias emacsd='/usr/bin/emacs --daemon &'
alias kemacs='killall emacs'

# Doom Emacs
alias doom='~/.emacs.d/bin/doom sync'
alias doomup='~/.emacs.d/bin/doom upgrade'
alias doomdoc='~/.emacs.d/bin/doom doctor'
alias doomrec='~/.emacs.d/bin/doom build'
alias doomref='~/.emacs.d/bin/doom refresh'

# MU4E
alias mu4emu='time mu init --maildir=~/Dokumenty/Maildir/'
alias mu4emb='time mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a'

# Git
alias gtc='git clone'

# grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# ledger
alias bal='ledger --empty -S -T -f ~/Dokumenty/ledger/ledger.dat bal ^assets:Santander assets:gotówka assets:drobne assets:SteamWallet assets:PayPal'
alias debt='ledger --empty -S -T -f ~/Dokumenty/ledger/debt.dat bal ^assets:Wiki'

# ls
alias ls='ls -la --color=always --group-directories-first'

# Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

# Music
alias mixer='ncpamixer'
alias music='mpd ~/.config/mpd/mpd.conf && yams -p 6601 -r && ncmpcpp'
alias ncpc='ncmpcpp'

# Mount
alias mountvm='sudo mount -t 9p -o trans=virtio /sharepoint share'

# mkdir
alias mkdir='mkdir -pv'

# Picom
alias kap='killall picom'
alias picom='picom --experimental-backend -b'

# Reboot
alias reboot='sudo reboot'
alias shutdown='sudo shutdown now'

# Timeshift
alias tsc='sudo timeshift --create'
alias tsl='sudo timeshift --list'
alias tgui='sudo timeshift-gtk'

# Vim
alias vconf='vim ~/.vimrc'
alias vfzf='vim $(fzf)'

# yt-dlp
alias yt='yt-dlp -x --audio-format flac --audio-quality 0'
alias ytv='yt-dlp -f mp4'
