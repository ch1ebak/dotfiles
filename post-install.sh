#!/bin/sh

echo -ne "
    ____   ____  _____ ______      ____ _   __ _____ ______ ___     __     __       _____  ______ ____   ____ ____  ______
   / __ \ / __ \/ ___//_  __/     /  _// | / // ___//_  __//   |   / /    / /      / ___/ / ____// __ \ /  _// __ \/_  __/
  / /_/ // / / /\__ \  / /______  / / /  |/ / \__ \  / /  / /| |  / /    / /       \__ \ / /    / /_/ / / / / /_/ / / /
 / ____// /_/ /___/ / / //_____/_/ / / /|  / ___/ / / /  / ___ | / /___ / /___    ___/ // /___ / _, _/_/ / / ____/ / /
/_/     \____//____/ /_/       /___//_/ |_/ /____/ /_/  /_/  |_|/_____//_____/   /____/ \____//_/ |_|/___//_/     /_/
"

# INSTALL APPS
## Pacman
LIST_OF_APPS_PACMAN="
alacritty
alsa-utils
bpytop
calibre
cantarell-fonts
dunst
emacs-nativecomp
fastfetch
fd
file-roller
firefox
fzf
gimp
git
gvfs
gvfs-mtp
keepassxc
less
lxappearance-gtk3
mpv
network-manager-applet
networkmanager
nitrogen
nsxiv
nvidia-open-dkms
nvidia-settings
nvtop
papirus-icon-theme
python-pip
python-psutil
qbittorrent
qtile
ripgrep
rofi
slock
spotify-launcher
steam
thunar
trash-cli
tlp
ttf-font-awesome
ttf-jetbrains-mono-nerd
wget
xorg-server
xorg-xrandr
xorg-xbacklight
xorg-xinit
xorg-xkill"

pacman -Syu
pacman -S $LIST_OF_APPS_PACMAN

echo -ne "Pacman done"

## Paru
# LIST_OF_APPS_PARU="
# ferdium-bin
# pandoc-bin
# picom-git
# protontricks
# slimbookbattery
# spicetify-cli"

# pacman -S --needed base-devel
# git clone https://aur.archlinux.org/paru.git
# cd paru
# makepkg -si

# echo -ne "Paru installed"

# paru -S $LIST_OF_APPS_PARU

# echo -ne "Paru done"

# DOTFILES
git clone https://github.com/ch1ebak/dotfiles
cd dotfiles
mv -i bashrc /home/$USER/.bashrc
mv -i Xresources /home/$USER/.Xresources
mv -i config/* /home/$USER/.config/
mv -i mozilla/backups /home/$USER/.mozilla/backups
cd ..
trash -vi dotfiles

echo -ne "Dotfiles done"

# FINISHING TOUCHES
trash /home/$USER/.bash_profile
echo -ne "xrdb ~/.Xresources" > .xinitrc
echo -ne "exec qtile start" >> .xinitrc
                                    
# cp assets/* /home/$USER/.local/share/

echo -ne "
    ___     __     __       ____   ____   _   __ ______
   /   |   / /    / /      / __ \ / __ \ / | / // ____/
  / /| |  / /    / /      / / / // / / //  |/ // __/
 / ___ | / /___ / /___   / /_/ // /_/ // /|  // /___
/_/  |_|/_____//_____/  /_____/ \____//_/ |_//_____/
Remember to reboot!
"
