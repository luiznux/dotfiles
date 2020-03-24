#!/bin/bash
#
#  /$$      /$$           /$$                       /$$      /$$ /$$$$$$$$          /$$$
# | $$$    /$$$          | $$                      | $$$    /$$$| $$_____/         |_  $$
# | $$$$  /$$$$  /$$$$$$ | $$   /$$  /$$$$$$       | $$$$  /$$$$| $$             /$$ \  $$
# | $$ $$/$$ $$ |____  $$| $$  /$$/ /$$__  $$      | $$ $$/$$ $$| $$$$$         |__/  | $$
# | $$  $$$| $$  /$$$$$$$| $$$$$$/ | $$$$$$$$      | $$  $$$| $$| $$__/               | $$
# | $$\  $ | $$ /$$__  $$| $$_  $$ | $$_____/      | $$\  $ | $$| $$             /$$  /$$/
# | $$ \/  | $$|  $$$$$$$| $$ \  $$|  $$$$$$$      | $$ \/  | $$| $$$$$$$$      |__//$$$/
# |__/     |__/ \_______/|__/  \__/ \_______/      |__/     |__/|________/         |___/
#
#
#

#define dotfiles path
dotfiles= $PWD

echo "#----------------------------------------------- Setup directory tree"
mkdir -vp ~/{Github/{luiznux,prog,other},AUR,Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,Documents,Desktop,projects,.vim,.config/{i3,polybar,ranger}}
cd ~/Github && git init
echo "                        Directory tree {OK}"

echo "#----------------------------------------------- Packages"
echo "     Installing packages"
sudo pacman -Sy xorg man gvim tree rxvt-unicode rxvt-unicode-terminfo urxvt-perls cmake libmpdclient wget i3-gaps i3lock-color ranger w3m nemo nemo-fileroller papirus-icon-theme sl feh vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts clang tlp i7z cpupower alsa alsa-utils alsa-firmware calcurse pulseaudio ttf-font-awesome libxss libcurl-gnutls dmenu mailutils llvm dhcp dhcpcd haveged xreader calibre ristretto tumbler evince playerctl check gobject-introspection transmission-gtk file ffmpegthumbnailer highlight atool imagemagick fftw openjdk11-src lxrandr-gtk3 mtpfs gvfs-mtp gvfs-gphoto2 android-file-transfer libmtp ufw sxiv yasm lxappearance gtk-chtheme xorg-xinit intltool dbus-glib gnome-shell gnome-session yelp-tools docbook-xsl
echo "     Packages {OK}"

echo "#----------------------------------------------- PYTHON CONFIG"
echo "     Python config"
sudo pacman -S python-pip python-sphinx python-dbus python2-gobject pygtk python-psutil python-urwid --noconfirm
echo "	   Python {OK}"

echo "#----------------------------------------------- Graphic drives and NVIDIA"
echo "	   Graphic drivers"
sudo pacman -S xf86-video-intel vulkan-intel mesa-demos nvidia nvidia-utils nvidia-settings bumblebee --noconfirm
echo "	   Graphic Drivers {OK}"

echo "#---------------------------------------- AUR packages"
echo "Installing some AUR Packages"

#AUR variables and func
exit_dir(){
 cd ..
}

AUR= ~/AUR/

echo "#------------- OPTIMUS MANAGER AND GDM"
#cd $AUR && git clone https://aur.archlinux.org/gdm3setup-utils.git && cd gdm3setup-utils/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/gdm-prime.git && cd gdm-prime/ && makepkg -i --noconfirm && exit_dir return
cd $AUR && git clone https://aur.archlinux.org/nvidia-xrun-pm.git && cd nvidia-xrun-pm/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/optimus-manager.git && cd optimus-manager/ && makepkg -i --noconfirm && exit_dir

#Batterymon and depence(LAPTOP ONLY)
cd $AUR && git clone https://aur.archlinux.org/python2-distutils-extra.git && cd python2-distutils-extra/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/batterymon-clone.git && cd batterymon-clone && makepkg -i --noconfirm && exit_dir

echo "  SPORIFY AND PACKAGES"
cd $AUR && git clone https://aur.archlinux.org/spotify.git && cd spotify && makepkg -i --noconfirm && exit_dir
gpg --keyserver pgp.mit.edu --recv-keys FCF986EA15E6E293A5644F10B4322F04D67658D8
cd $AUR && git clone https://aur.archlinux.org/ffmpeg-compat-57.git && cd ffmpeg-compat-57 && makepkg -i --noconfirm && exit_dir

#------------ Other packages
cd $AUR && git clone https://aur.archlinux.org/python-pdftotext.git && cd python-pdftotext/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/polybar.git && cd polybar/ && makepkg -i
cd $AUR && git clone https://aur.archlinux.org/thermald.git && cd thermald/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/ttf-weather-icons.git && cd ttf-weather-icons.git/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/wps-office.git && cd wps-office/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/ttf-wps-fonts.git && cd ttf-wps-fonts && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/qdirstat.git && cd qdirstat/ && makepkg -i --noconfirm && exit_dir --noconfirm
cd $AUR && git clone https://aur.archlinux.org/jmtpfs.git && cd jmtpfs/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/sublime-text-dev.git && cd sublime-text-dev/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/speedometer.git && cd speedometer/ && makepkg -i --noconfirm && exit_dir
cd $AUR && git clone https://aur.archlinux.org/cli-visualizer.git && cd cli-visualizer/ && makepkg -i --noconfirm && exit_dir

echo "#----------------------------------------laptop config"
sudo pacman -S acpi libinput xf86-input-synaptics xorg-xinput powertop xfce4-power-manager bluez bluez-utils bbswitch --noconfirm

echo "#----------------------------------------EMACS INSTALL"
cd $dotfiles && cp -r emacs/.emacs.d  ~/.emacs.d/
echo "     Emacs config {OK} "
cd ~/ && wget gnu.c3sl.ufpr.br/ftp/emacs/emacs-26.3.tar.xz && tar -xvf emacs-26.3.tar.xz && rm emacs-26.3.tar.xz
cd ~/emacs-26.3 && ./autogen.sh && ./configure && make && sudo make install
echo "     Emacs  Install  {OK}"

echo "#----------------------------------------Setup Pacman config"
cd $dotfiles && sudo cp config/pacman/mirrorlist /etc/pacman.d/
sudo rm /etc/pacman.conf || cd $dotfiles && sudo cp config/pacman/pacman.conf  /etc/

echo "#----------------------------------------Setup i3 and polybar"
cd $dotfiles && cp config/i3/config ~/.config/i3/
cd $dotfiles && cp -R config/polybar/  ~/.config/polybar/
echo "     I3 and Polybar config {OK} "

echo "#----------------------------------------Polyabar Scripts"
cd ~/.config/polybar/modules/ && git clone https://github.com/kamek-pf/polybar-forecast.git
cd ~/.config/polybar/modules/polybar-forecast/ && cargo build --release
echo"	   Scripts    {OK}"

echo "#----------------------------------------Ranger config"
mkdir ~/.config/ranger
cd $dotfiles && cp config/rc.conf  ~/.config/ranger/
echo "     Ranger config file setup {OK} "

echo "#----------------------------------------Vim config setup"
mkdir ~/.vim
cd $dotfiles && cp vim/.vimrc ~/.vimrc
cd $dotfiles && cp -r vim/.vim/ ~/
echo "     Vim setup {OK} "

echo "#----------------------------------------Light(brithness control)"
cd ~/Github/prog/ && git clone https://github.com/haikarainen/light
cd ~/Github/prog/light && ./autogen.sh && ./configure && sudo make

echo "#----------------------------------------Setup font"
cd $dotfiles && sudo cp -R config/fonts/source-code-pro /usr/local/share/fonts/
echo "     Fount setup {OK} "

echo "#----------------------------------------Setup Xresources"
cd $dotfiles && cp config/.Xresources ~/.Xresources  && xrdb -laod ~/.Xresources
echo "     Xresources setup and loaded {OK} "

echo "#----------------------------------------Setup gitignore global file"
cd $dotfiles && cp config/.gitignore_global  ~/
echo "       Gitignore global setup {OK} "
cd $dotfiles && cp config/.gitconfig ~/
echo "       Gitconfig setup {OK} "

echo"#----------------------------------------Setup background image"
cd $dotfiles && cp config/blue-hair-girl.jpg ~/.config/wallpaper.jpg
echo "       Wallppaer setup {OK} "

echo"#----------------------------------------Setup Themes"
cd $dotfiles && cd gtk-themes/gtk && tar -xvf Midnight-BlueNight-Theme.tar.xz && sudo mv Midnight-BlueNight-Theme/  /usr/share/themes/
cd $dotfiles && cd config/ && cp -r gtk-2.0 gtk-3.0 ~/.config
cd $dotfiles && cd config/ && cp .gtkrc-2.0 ~/.gtkrc-2.0
gsettings set org.cinnamon.desktop.default-applications.terminal exec urxvt
echo "   GTK themes setup {OK} "

echo "#----------------------------------------Other Configs "
cd $dotfiles && cd config/ && cp .bashrc ~/
cd $dotfiles && cd config/ sudo rm /etc/tlp.conf && sudo cp tlp.conf /etc/tlp.conf

echo "#----------------------------------------BUMBLEBEE CONFIG (LAPTOP ONLY)"
sudo gpasswd -a user bumblebee
cd $dotfiles && sudo cp /config/bbswitch.conf /etc/modprobe.d/bbswitch.conf
tee /proc/acpi/bbswitch <<<OFF
sudo systemctl enable bumblebeed.service

echo "#----------------------------------------SENABLE YSTEMCTL SERVICES"
sudo systemctl enable NetworkManager.service
sudo systemctl enable gdm.service
sudo systemctl enable tlp.service
sudo systemctl enable ufw.service && ufw enable #firewall enable
#REMEMBER TO ENABLE OPTIMUS MANAGER
sudo systemctl enable optimus-manager.service

