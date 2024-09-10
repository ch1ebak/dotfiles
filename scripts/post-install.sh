#!/bin/sh

echo -e "
                                                                                        
========================================================================================
                                                                                        
▗▄▄▖  ▗▄▖  ▗▄▄▖▗▄▄▄▖▗▄▄▄▖▗▖  ▗▖ ▗▄▄▖▗▄▄▄▖▗▄▖ ▗▖   ▗▖        ▗▄▄▖ ▗▄▄▖▗▄▄▖ ▗▄▄▄▖▗▄▄▖▗▄▄▄▖
▐▌ ▐▌▐▌ ▐▌▐▌     █    █  ▐▛▚▖▐▌▐▌     █ ▐▌ ▐▌▐▌   ▐▌       ▐▌   ▐▌   ▐▌ ▐▌  █  ▐▌ ▐▌ █
▐▛▀▘ ▐▌ ▐▌ ▝▀▚▖  █    █  ▐▌ ▝▜▌ ▝▀▚▖  █ ▐▛▀▜▌▐▌   ▐▌        ▝▀▚▖▐▌   ▐▛▀▚▖  █  ▐▛▀▘  █
▐▌   ▝▚▄▞▘▗▄▄▞▘  █  ▗▄█▄▖▐▌  ▐▌▗▄▄▞▘  █ ▐▌ ▐▌▐▙▄▄▖▐▙▄▄▖    ▗▄▄▞▘▝▚▄▄▖▐▌ ▐▌▗▄█▄▖▐▌    █
                                                                                        
I sure hope it works!
                                                                                        
========================================================================================
                                                                                        
"

# INSTALL APPS
## Pacman
echo -e "
    ____   ___    ______ __  ___ ___     _   __   ___     ____   ____  _____
   / __ \ /   |  / ____//  |/  //   |   / | / /  /   |   / __ \ / __ \/ ___/
  / /_/ // /| | / /    / /|_/ // /| |  /  |/ /  / /| |  / /_/ // /_/ /\__ \ 
 / ____// ___ |/ /___ / /  / // ___ | / /|  /  / ___ | / ____// ____/___/ / 
/_/    /_/  |_|\____//_/  /_//_/  |_|/_/ |_/  /_/  |_|/_/    /_/    /____/  
                                                                            
"

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
linux-headers
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
ttf-font-awesome
ttf-jetbrains-mono-nerd
wget
xorg-server
xorg-xrandr
xorg-xbacklight
xorg-xinit
xorg-xkill"

sudo sed -Ei 's/^#(Color)$/\1\nILoveCandy/;s/^#(ParallelDownloads).*/\1 = 10/' /etc/pacman.conf
sudo pacman -Syu
sudo pacman -S $LIST_OF_APPS_PACMAN

## Paru
echo -e "
    ____   ___     ____   __  __   ___     ____   ____  _____
   / __ \ /   |   / __ \ / / / /  /   |   / __ \ / __ \/ ___/
  / /_/ // /| |  / /_/ // / / /  / /| |  / /_/ // /_/ /\__ \ 
 / ____// ___ | / _, _// /_/ /  / ___ | / ____// ____/___/ / 
/_/    /_/  |_|/_/ |_| \____/  /_/  |_|/_/    /_/    /____/  
                                                             
"

LIST_OF_APPS_PARU="
ferdium-bin
pandoc-bin
picom-git
protontricks
spicetify-cli"

sudo pacman -S --needed base-devel
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si

paru -S $LIST_OF_APPS_PARU
cd ..
trash -v /home/$USER/paru

# DOTFILES
echo -e "
    ____   ____  ______ ______ ____ __     ______ _____
   / __ \ / __ \/_  __// ____//  _// /    / ____// ___/
  / / / // / / / / /  / /_    / / / /    / __/   \__ \ 
 / /_/ // /_/ / / /  / __/  _/ / / /___ / /___  ___/ / 
/_____/ \____/ /_/  /_/    /___//_____//_____/ /____/  
                                                       
"

git clone https://github.com/ch1ebak/dotfiles
cd dotfiles
mv -f bashrc /home/$USER/.bashrc
mv -f Xresources /home/$USER/.Xresources
trash -v /home/$USER/.config/qtile
mv -f config/* /home/$USER/.config/
mv -f mozilla/backups /home/$USER/.mozilla/backups
cd ..
trash -v dotfiles

# FINISHING TOUCHES
trash -v /home/$USER/.bash_profile
echo -e "xrdb ~/.Xresources" > .xinitrc
echo -e "exec qtile start" >> .xinitrc
                                    
# cp assets/* /home/$USER/.local/share/

echo -e "
                                                                                        
========================================================================================
                                                                                        
 ▗▄▖ ▗▖   ▗▖       ▗▄▄▄  ▗▄▖ ▗▖  ▗▖▗▄▄▄▖
▐▌ ▐▌▐▌   ▐▌       ▐▌  █▐▌ ▐▌▐▛▚▖▐▌▐▌
▐▛▀▜▌▐▌   ▐▌       ▐▌  █▐▌ ▐▌▐▌ ▝▜▌▐▛▀▀▘
▐▌ ▐▌▐▙▄▄▖▐▙▄▄▖    ▐▙▄▄▀▝▚▄▞▘▐▌  ▐▌▐▙▄▄▖
                                                                                        
I sure hope it works!
                                                                                        
========================================================================================
                                                                                        
"
