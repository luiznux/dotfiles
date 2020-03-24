#!bin/bash
#
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

#Variables
dotfiles=$(pwd)
AUR=~/AUR


#break line with echo command
break_line(){
    echo ""
}


#print erro mgs
erro_mgs(){
    echo "  ERROR" && break_line
}


#exit dir func
exit_dir(){
    cd ..
}


####func to setup my directory tree
dir_tree(){

    echo "#----------------------------------------------- Setup directory tree"
    mkdir -vp ~/{Github/{luiznux,prog,other},AUR,Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,Pictures,Documents,Desktop,projects,.vim,.config/{i3,polybar,ranger}} \
    && cd ~/Github && git init\
    echo "                        Directory tree {OK}" && brek_line || erro_mgs
}


####func to install all packages on arch linux
install_packges(){

    echo "#----------------------------------------------- Packages"
    echo "     Installing packages"
    sudo pacman -Sy xorg xclip man gvim tree neofetch firefox rxvt-unicode rxvt-unicode-terminfo urxvt-perls powerline cmake libmpdclient wget i3-gaps i3lock-color ranger w3m nemo nemo-fileroller papirus-icon-theme sl feh vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts clang tlp i7z cpupower alsa alsa-utils alsa-firmware calcurse pulseaudio ttf-font-awesome libxss libcurl-gnutls dmenu mailutils llvm dhcp dhcpcd haveged xreader calibre ristretto tumbler evince playerctl check gobject-introspection transmission-gtk file ffmpegthumbnailer highlight atool imagemagick fftw openjdk11-src lxrandr-gtk3 mtpfs gvfs-mtp gvfs-gphoto2 android-file-transfer libmtp ufw sxiv yasm lxappearance gtk-chtheme xorg-xinit intltool dbus-glib gnome-shell gnome-session yelp-tools docbook-xsl \
         && echo "     Packages {OK}" && brek_line || erro_mgs
}


####func to install my python packages
Python_config(){
    echo "#----------------------------------------------- PYTHON CONFIG"
    echo "     Python config"
    sudo pacman -S python-pip python-sphinx python-dbus python2-gobject pygtk python-psutil python-urwid --noconfirm \
        && echo "	    Python {OK}" && break_line || erro_mgs
}


####func to install the graphic drivers
Graphic_drivers(){

    echo "#----------------------------------------------- Graphic drives and NVIDIA" && break_line
    echo "	   Graphic drivers" && break_line
    sudo pacman -S xf86-video-intel vulkan-intel mesa-demos nvidia nvidia-utils nvidia-settings bumblebee --noconfirm \
        && echo "	     Graphic Drivers {OK}" && break_line || erro_mgs
}


####AUR Packges installation func
AUR_install(){

    echo "#---------------------------------------- AUR packages" && break_line
    echo "Installing some AUR Packages" && break_line

    echo "#OPTIMUS MANAGER AND GDM" && break_line
    echo " gdm-prime optimus-manager optimus-manager-qt"
    cd $AUR && git clone https://aur.archlinux.org/gdm-prime.git && cd gdm-prime/ && makepkg -i --noconfirm && exit_dir\
    && cd $AUR && git clone https://aur.archlinux.org/optimus-manager.git && cd optimus-manager/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/optimus-manager-qt.git && cd optimus-manager-qt/ && makepkg -i --noconfirm && exit_dir \
    && echo "Done" && break_line || erro_mgs

    echo "  SPORIFY AND PACKAGES"
    cd $AUR && git clone https://aur.archlinux.org/spotify.git && cd spotify && makepkg -i --noconfirm && exit_dir \
    && gpg --keyserver pgp.mit.edu --recv-keys FCF986EA15E6E293A5644F10B4322F04D67658D8 \
    && cd $AUR && git clone https://aur.archlinux.org/ffmpeg-compat-57.git && cd ffmpeg-compat-57 && makepkg -i --noconfirm && exit_dir \
    && echo "Done" && break_line || erro_mgs

    echo "#------------ Other packages" && break_line
    echo " nvidia-xrun-pm python-pdftotext polybar thermald ttf-weather-icon wps-office.git "
    echo " ttf-wps-fonts qdirstat jmtpfs sublime-text-dev speedometer cli-visualizer"
    cd $AUR && git clone https://aur.archlinux.org/nvidia-xrun-pm.git && cd nvidia-xrun-pm/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/python-pdftotext.git && cd python-pdftotext/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/polybar.git && cd polybar/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/thermald.git && cd thermald/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/ttf-weather-icons.git && cd ttf-weather-icons.git/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/wps-office.git && cd wps-office/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/ttf-wps-fonts.git && cd ttf-wps-fonts && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/qdirstat.git && cd qdirstat/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/jmtpfs.git && cd jmtpfs/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/sublime-text-dev.git && cd sublime-text-dev/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/speedometer.git && cd speedometer/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/cli-visualizer.git && cd cli-visualizer/ && makepkg -i --noconfirm && exit_dir \
    && echo " Done" && break_line || erro_mgs
}


####Emacs install and mv config
emacs(){

    echo "#----------------------------------------EMACS INSTALL" && break_line
    cd $dotfiles && cp -r emacs/.emacs.d  ~/.emacs.d/ \
    && echo "     Emacs config {OK} " && break_line || erro_mgs

    cd ~/ && wget gnu.c3sl.ufpr.br/ftp/emacs/emacs-26.3.tar.xz && tar -xvf emacs-26.3.tar.xz && rm emacs-26.3.tar.xz \
    && cd ~/emacs-26.3 && ./autogen.sh && ./configure && make && sudo make install \
    && echo "     Emacs  Install  {OK}" && break_line || erro_msg
}


####mv all the others dotfiles
general_config(){

    echo "#----------------------------------------Setup i3 and polybar" && break_line
    cd $dotfiles && cp i3/config ~/.config/i3/ \
    && cd $dotfiles && cp -r polybar/*  ~/.config/polybar/ \
    && echo "     I3 and Polybar config {OK} " && break_line || erro_msg

    echo "#----------------------------------------Polyabar Scripts" && break_line
    cd ~/.config/polybar/modules/ && git clone https://github.com/kamek-pf/polybar-forecast.git \
    && cd ~/.config/polybar/modules/polybar-forecast/ && cargo build --release \
    && echo"	   Scripts {OK}" && break_line || erro_msg

    echo "#----------------------------------------Ranger config" && break_line
    mkdir ~/.config/ranger \
    && cd $dotfiles && cp config/rc.conf  ~/.config/ranger/ \
    && echo "     Ranger config file setup {OK} " && break_line || erro_msg

    echo "#----------------------------------------Vim config setup" && break_line
    mkdir ~/.vim \
    && cd $dotfiles && cp vim/.vimrc ~/.vimrc \
    && cd $dotfiles && cp -r vim/.vim/ ~/ \
    && echo "     Vim setup {OK} " && break_line || erro_msg

    echo "#----------------------------------------Setup font" && break_line
    cd $dotfiles && sudo cp -R config/fonts/source-code-pro /usr/local/share/fonts/ \
    && echo "     Fount setup {OK} " && break_line || erro_msg

    echo"#----------------------------------------Setup Locale" && break_line
    cd $dotfiles && sudo cp config/locale.conf  /etc/ && sudo locale-gen \
    && echo "      Locale setup {OK}" && break_line || erro_msg

    echo "#----------------------------------------Setup Xresources" && break_line
    cd $dotfiles && cp config/.Xresources ~/.Xresources  && xrdb -laod ~/.Xresources \
    && echo "     Xresources setup and loaded {OK} " && break_line || erro_msg

    echo "#----------------------------------------Setup gitignore global file" && break_line
    cd $dotfiles && cp config/.gitignore_global  ~/ \
    && echo "     Gitignore global setup {OK} " && break_line || erro_msg
    cd $dotfiles && cp config/.gitconfig ~/ \
    && echo "     Gitconfig setup {OK} " && break_line || erro_msg

    echo"#----------------------------------------Setup background image" && break_line
    cd $dotfiles && cp config/blue-hair-girl.jpg ~/.config/wallpaper.jpg  \
    && echo "      Wallppaer setup {OK} " && break_line || erro_msg

    echo"#----------------------------------------Setup Themes" && break_line
    #cd $dotfiles && cd gtk-themes/gtk && tar -xvf Midnight-BlueNight-Theme.tar.xz && sudo mv Midnight-BlueNight-Theme/  /usr/share/themes/
    cd $dotfiles && cd config/ && cp -r gtk-2.0 gtk-3.0 ~/.config \
    && cd $dotfiles && cd config/ && cp .gtkrc-2.0 ~/.gtkrc-2.0 \
    && echo "      GTK themes setup {OK} " && break_line || erro_msg

    echo "#----------------------------------------Setup Pacman config" && break_line
    cd $dotfiles && sudo cp config/pacman/mirrorlist /etc/pacman.d/ \
    && sudo rm /etc/pacman.conf || cd $dotfiles && sudo cp config/pacman/pacman.conf  /etc/ \
    && cd echo "     Pacman config {OK} " && break_line || erro_msg

    echo "#----------------------------------------Other Configs " && break_line
    cd $dotfiles && cd config/ && cp .bashrc ~/ \
    && cd $dotfiles && cd config/ sudo rm /etc/tlp.conf && sudo cp tlp.conf /etc/tlp.conf \
    && cd $dotfiles && sudo cp config/X11/xinit/xinitrc /etc/X11/xinit/ \
    && echo "      Done" && break_line || erro_msg
}


####func that install laptoptools
laptop_config(){

    echo "#----------------------------------------laptop packges" && break_line
    sudo pacman -S acpi libinput xf86-input-synaptics xorg-xinput powertop xfce4-power-manager bluez bluez-utils bbswitch --noconfirm \
    && echo " Done" && break_line || erro_msg

    echo "#----------------------------------------BUMBLEBEE CONFIG (LAPTOP ONLY)" && break_line
    sudo gpasswd -a luiznux bumblebee \
    && cd $dotfiles && sudo cp /config/bbswitch.conf /etc/modprobe.d/bbswitch.conf \
    && tee /proc/acpi/bbswitch <<<OFF \
    && sudo systemctl enable bumblebeed.service \
    echo "Bumblebee {OK}" && break_line || erro_msg

    echo "#----------------------------------------Light(brithness control)" && break_line
    cd ~/Github/prog/ && git clone https://github.com/haikarainen/light \
    && cd ~/Github/prog/light && ./autogen.sh && ./configure && sudo make \
    && echo "     Light {OK}" && break_line || erro_msg

    #Batterymon and depence(LAPTOP ONLY)
    cd $AUR && git clone https://aur.archlinux.org/python2-distutils-extra.git && cd python2-distutils-extra/ && makepkg -i --noconfirm && exit_dir \
    && cd $AUR && git clone https://aur.archlinux.org/batterymon-clone.git && cd batterymon-clone && makepkg -i --noconfirm && exit_dir \
    && echo "Laptop configs {OK}" && break_line || erro_msg
}


####func that enable some services on systemd
systemd_init(){

    echo "#----------------------------------------SENABLE YSTEMCTL SERVICES" && break_line
    sudo systemctl enable NetworkManager.service \
    && sudo systemctl enable gdm.service \
    && sudo systemctl enable tlp.service \
    && sudo systemctl enable ufw.service && ufw enable \
    && sudo systemctl enable optimus-manager.service \
    && echo "Done" && break_line || erro_msg
}


################################MAIN
dir_tree
install_packges
Python_config
Graphic_drivers
AUR_install
emacs
general_config
laptop_config
systemd_init