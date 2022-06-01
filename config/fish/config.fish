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

## Fetch
# colorscript random
# neofetch
# pfetch
# ppfetch
starfetch

# Starship
starship init fish | source

## Autocomplete and highlight colors
# Catppuccin
# set fish_color_normal '#dadae8'
# set fish_color_autosuggestion '#6e6c7e'
# set fish_color_command '#a4b9ef'
# set fish_color_error '#e38c8f'
# set fish_color_param '#f2cecf'

# Dracula
set fish_color_normal '#f8f8f2'
set fish_color_autosuggestion '#6272a4'
set fish_color_command '#8be9fd'
set fish_color_error '#ff5555'
set fish_color_param '#bd93f9'

# Graphite
# set fish_color_normal '#f7f7f7'
# set fish_color_autosuggestion '#525252'
# set fish_color_command '#f7f7f7'
# set fish_color_error '#bf616a'
# set fish_color_param '#f7f7f7'

# Nord
# set fish_color_normal '#eceff4'
# set fish_color_autosuggestion '#4c566a'
# set fish_color_command '#eceff4'
# set fish_color_error '#bf616a'
# set fish_color_param '#eceff4'


### ALIASES

## arch - pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -R'
alias gy='yay -S'
alias ry='yay -R'
alias pacup='sudo pacman -Syu'                  # update only standard pkgs
alias yayup='yay -Syu'                          # update standard pkgs and AUR pkgs (yay)
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/kodowanie/packages.txt'

##void - xbps and xi
# alias gp='sudo xbps-install'
# alias rp='sudo xbps-remove -R'
# alias cleanup='sudo xbps-remove -Oo'
# alias xi='sudo xi'
# alias xsu='sudo xbps-install -Su'

## Bpytop
alias bt='bpytop'

## confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

## Curl
alias ccs='curl cheat.sh'
alias cip='curl ifconfig.co'
alias cnws='curl pl.getnews.tech'
alias cbtc='curl rate.sx'
alias cwpl='curl wttr.in/pleszew'
alias cwpo='curl wttr.in/poznan'
alias cwv='curl wttr.in/irvine'

## Emacs
alias em='/usr/bin/emacs -nw'
alias emacsd='/usr/bin/emacs --daemon &'
alias kemacs='killall emacs'

# Doom Emacs
alias doom='~/.emacs.d/bin/doom sync'
alias doomup='~/.emacs.d/bin/doom upgrade'
alias doomdoc='~/.emacs.d/bin/doom doctor'
alias doomrec='~/.emacs.d/bin/doom build'

# MU4E
alias mu4emu='time mu init --maildir=~/Dokumenty/org/Maildir/'
alias mu4emb='time mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a'

## grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

## Changing "ls" to "exa"
alias ls='ls -la --color=always --group-directories-first' # my preferred listing

## Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

## Mixer
alias mixer='ncpamixer'

## Mount
alias mountvm='sudo mount -t 9p -o trans=virtio /sharepoint share'

## mkdir
alias mkdir='mkdir -pv'

## Neovim
alias vim='nvim'
alias neovim='nvim'

## Picom
alias kap='killall picom'
alias picom='picom --experimental-backend -b'

## Reboot
alias reboot='sudo reboot'
alias shutdown='sudo shutdown now'

## Timeshift
alias tsc='sudo timeshift --create'
alias tsl='sudo timeshift --list'
alias tgui='sudo timeshift-gtk'

## Uptime
alias upt='uptime'

## youtube-dl
alias ydla='youtube-dl --extract-audio --audio-format best'
alias ydlv='youtube-dl -f bestvideo+bestaudio'
