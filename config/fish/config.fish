###################################
### https://github.com/ch1ebak/ ###
###################################


### EYE CANDY
# fish_config theme choose catppuccin
fish_config theme choose dracula
# fish_config theme choose everforest
# fish_config theme choose grayscale
# fish_config theme choose gruvbox
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

## FZF
set -Ux FZF_DEFAULT_OPTS "--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4"


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
alias allup='yay -Syu'

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

# Df
alias df='duf'

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
alias dots='git clone https://github.com/ch1ebak/dotfiles'
alias stpage='git clone https://github.com/ch1ebak/ch1ebak.github.io'

# grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# ledger
alias bal='ledger --empty -S -T -f ~/Dokumenty/ledger/ledger.dat bal ^assets:Santander assets:gotówka assets:drobne assets:SteamWallet assets:PayPal'

# ls
alias ls='ls -la --color=always --group-directories-first'

# Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

# Music
alias mixer='ncpamixer'
alias music='mpd ~/.config/mpd/mpd.conf && yams -p 6600 -r && ncmpcpp'
alias musicel='mpd ~/.config/mpd/mpd.conf && yams -p 6600 -r'
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
alias vfzf='vim $(fzf)'
alias vconf='vim ~/.vimrc'
alias vbash='vim ~/.bashrc'
alias vxinit='vim ~/.xinitrc'
alias vxres='vim ~/.Xresources'
alias vstarship='vim ~/.config/starship.toml'
alias vcritty='vim ~/.config/alacritty/alacritty.yml'
alias vbtop='vim ~/.config/bpytop/bpytop.conf'
alias vdunst='vim ~/.config/dunst/dunstrc'
alias vfish='vim ~/.config/fish/config.fish'
alias vkitty='vim ~/.config/kitty/kitty.conf'
alias vmpd='vim ~/.config/mpd/mpd.conf'
alias vncpc='vim ~/.config/ncmpcpp/config'
alias vnfetch='vim ~/.config/neofetch/config.conf'
alias vpicom='vim ~/.config/picom/picom.conf'
alias vqtile='vim ~/.config/qtile/config.py'
alias vzathura='vim ~/.config/zathura/zathurarc'

# yt-dlp
alias yt='yt-dlp -x --audio-format flac --audio-quality 0'
alias ytv='yt-dlp -f mp4'
