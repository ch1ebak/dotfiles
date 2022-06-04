# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

### Options

# Archives
ex ()
{
  if [ -f "$1" ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}


# Path
if [ -d "$HOME/.bin" ] ;
  then PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/Applications" ] ;
  then PATH="$HOME/Applications:$PATH"
fi

# Vi mode
set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'


### EYE CANDY

# Starship
eval "$(starship init bash)"

# Fetch
colorscript random
# neofetch
# pfetch
# ppfetch
# starfetch


### ALIASES

# arch - pacman and yay
alias gp='sudo pacman -S'
alias rp='sudo pacman -R'
alias gy='yay -S'
alias ry='yay -R'
alias pacup='sudo pacman -Syu'                  # update only standard pkgs
alias yayup='yay -Syu'                          # update standard pkgs and AUR pkgs (yay)
alias cleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages
alias pkglist='sudo pacman -Qqe > ~/Dokumenty/kodowanie/packages.txt'
alias pkgcount='pacman -Q | wc -l'

# Bpytop
alias bt='bpytop'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# Curl
alias cwpl='curl wttr.in/pleszew'
alias cwpo='curl wttr.in/poznan'

# Emacs
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

# grub
alias grubreload='sudo grub-mkconfig -o /boot/grub/grub.cfg'

# Changing "ls" to "exa"
alias ls='ls -la --color=always --group-directories-first' # my preferred listing

# Merge Xresources
alias merge='xrdb -merge ~/.Xresources'

# Mixer
alias mixer='ncpamixer'

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

# Uptime
alias upt='uptime'
