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
#

echo "#----------------------------------------- Setup directory tree"
mkdir -p ~/Github/{luiznux,prog,other},AUR/{Done,Making},Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,Documents,Desktop,projects,.vim,.config/{i3,polybar,ranger}
cd ~/Github && git init
echo "    Directory tree {OK}"

echo "#----------------------------------------- Packages"
echo "		Installing packages"
sudo pacman -S  man gvim tree rxvt-unicode rxvt-unicode-terminfo urxvt-perls cmake libmpdclient wget i3-gaps i3lock-color ranger w3m xorg xorg-xinit nemo nemo-fileroller papirus-icon-theme sl feh vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts clang tlp i7z cpupower alsa calcurse pulseaudio ttf-font-awesome libxss libcurl-gnutls dmenu mailutils llvm dhcp dhcpcd haveged  xreader calibre ristretto tumbler evince playerctl check gobject-introspection transmission-gtk file ffmpegthumbnailer highlight atool imagemagick fftw openjdk11-src lxrandr-gtk3 mtpfs gvfs-mtp gvfs-gphoto2 android-file-transfer libmtp ufw sxiv yasm lxappearance gtk-chtheme
echo " 	Packages {OK}"

echo "#---------------------------------------- PYTHON CONFIG"
echo "		Python config"
sudo pacman -S python-pip python-sphinx python-dbus python2-gobject pygtk python-psutil python-urwid \
echo "		Python {OK}"

echo "#---------------------------------------- Graphic drives and NVIDIA"
echo "		Graphic drivers"
sudo pacman -S xf86-video-intel vulkan-intel nvidia nvidia-utils nvidia-settings bumblebee
echo "		Graphic Drivers {OK}"

echo "#---------------------------------------- AUR packages"
echo "Installing some AUR Packages"

#AUR variables
AUR  = cd ~/AUR

echo "#------------- OPTIMUS MANAGER AND GDM"
$AUR && git clone https://aur.archlinux.org/gdm3setup-utils.git && cd gdm3setup-utils/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/packages/gdm-prime.git && cd gdm-prime/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/nvidia-xrun-pm.git && cd nvidia-xrun-pm/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/optimus-manager.git && cd optimus-manager/ && makepkg -i

#Batterymon and depence(LAPTOP ONLY)
$AUR && git clone https://aur.archlinux.org/python2-distutils-extra.git && cd python2-distutils-extra/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/batterymon-clone.git && cd batterymon-clone && makepkg -i

echo "  SPORIFY AND PACKAGES"
gpg --keyserver pgp.mit.edu --recv-keys FCF986EA15E6E293A5644F10B4322F04D67658D8
$AUR && git clone https://aur.archlinux.org/ffmpeg-compat-57.git && cd ffmpeg-compat-57 && makepkg -i
$AUR && git clone https://aur.archlinux.org/spotify.git && cd spotify && makepkg -i

#------------ Other packages
$AUR && git clone https://aur.archlinux.org/python-pdftotext.git && cd python-pdftotext/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/polybar.git && cd polybar/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/thermald.git && cd thermald/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/ttf-weather-icons.git && cd ttf-weather-icons.git/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/wps-office.git && cd wps-office/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/ttf-wps-fonts.git && cd ttf-wps-fonts && makepkg -i
$AUR && git clone https://aur.archlinux.org/qdirstat.git && cd qdirstat/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/jmtpfs.git && cd jmtpfs/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/sublime-text-dev.git && cd sublime-text-dev/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/speedometer.git && cd speedometer/ && makepkg -i
$AUR && git clone https://aur.archlinux.org/cli-visualizer.git && cd cli-visualizer/ && makepkg -i

echo "#----------------------------------------laptop config"
sudo pacman -S acpi libinput xf86-input-synaptics xorg-xinput powertop xfce4-power-manager bluez bluez-utils bbswitch

echo "#----------------------------------------EMACS INSTALL"
cp -r emacs/.emacs.d  ~/.emacs.d/
echo "          Emacs config {OK} "
cd ~/ && wget gnu.c3sl.ufpr.br/ftp/emacs/emacs-26.3.tar.xz && tar -xvf emacs-26.3.tar.xz && rm emacs-26.3.tar.xz
cd ~/emacs-26.3 && ./autogen.sh && ./configure && make && sudo make install
echo "        Emacs  Install  {OK}"

echo "#----------------------------------------Setup i3 and polybar"
cp config/i3/config ~/.config/i3/
cp -R config/polybar/  ~/.config/polybar/
echo "        I3 and Polybar config {OK} "

echo "#----------------------------------------Polyabar Scripts"
cd ~/.config/polybar/modules/ && git clone https://github.com/kamek-pf/polybar-forecast.git
cd ~/.config/polybar/modules/polybar-forecast/ && cargo build --release
echo"				   	Scripts    {OK}"

echo "#----------------------------------------Ranger config"
mkdir ~/.config/ranger
cp config/rc.conf  ~/.config/ranger/
echo "     Ranger config file setup {OK} "

echo "#----------------------------------------Vim config setup"
mkdir ~/.vim
cp vim/.vimrc ~/.vimrc
cp -r vim/.vim/ ~/
echo "             Vim setup {OK} "

echo "#----------------------------------------Light(brithness control)"
cd ~/Github/prog/ && git clone https://github.com/haikarainen/light
cd ~/Github/prog/light && ./autogen.sh && ./configure && sudo make

echo "#----------------------------------------Setup font"
sudo cp -R config/fonts/source-code-pro /usr/local/share/fonts/
echo "           Fount setup {OK} "

echo "#----------------------------------------Setup Xresources"
cp config/.Xresources ~/.Xresources  && xrdb -laod ~/.Xresources
echo "     Xresources setup and loaded {OK} "

echo "#----------------------------------------Setup gitignore global file"
cp config/.gitignore_global  ~/
echo "       Gitignore global setup {OK} "
cp config/.gitconfig ~/
echo "              Gitconfig setup {OK} "

echo"#----------------------------------------Setup background image"
cp config/blue-hair-girl.jpg ~/.config/wallpaper.jpg
echo "              Wallppaer setup {OK} "

echo"#----------------------------------------Setup Themes"
cd gtk-themes/gtk && tar -xvf Midnight-BlueNight-Theme.tar.xz && sudo mv Midnight-BlueNight-Theme/  /usr/share/themes/
cd config/ && cp -r gtk-2.0 gtk-3.0 ~/.config
cd config/ && cp .gtkrc-2.0 ~/.gtkrc-2.0
gsettings set org.cinnamon.desktop.default-applications.terminal exec urxvt
echo "   GTK themes setup {OK} "

echo "#----------------------------------------Other Configs "
cd config/ && cp .bashrc ~/
cd config/ sudo rm /etc/tlp.conf && sudo cp tlp.conf /etc/tlp.conf

echo "#----------------------------------------BUMBLEBEE CONFIG (LAPTOP ONLY)"
sudo gpasswd -a user bumblebee
sudo cp /config/bbswitch.conf /etc/modprobe.d/bbswitch.conf
tee /proc/acpi/bbswitch <<<OFF
sudo systemctl enable bumblebeed.service

echo "#----------------------------------------SENABLE YSTEMCTL SERVICES"
sudo systemctl enable NetworkManager.service
sudo systemctl enable gdm.service
sudo systemctl enable tlp.service
#Remenber to enable firewall $ ufw enable
sudo systemctl enable ufw.service
#REMEMBER TO ENABLE OPTIMUS MANAGER
sudo systemctl enable optimus-manager.service

