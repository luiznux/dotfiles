#!/bin/bash
#
#     ██╗███╗   ██╗███████╗████████╗ █████╗ ██╗     ██╗
#     ██║████╗  ██║██╔════╝╚══██╔══╝██╔══██╗██║     ██║
#     ██║██╔██╗ ██║███████╗   ██║   ███████║██║     ██║
#     ██║██║╚██╗██║╚════██║   ██║   ██╔══██║██║     ██║
#     ██║██║ ╚████║███████║   ██║   ██║  ██║███████╗███████╗
#     ╚═╝╚═╝  ╚═══╝╚══════╝   ╚═╝   ╚═╝  ╚═╝╚══════╝╚══════╝
#
#   ██████╗  ██████╗ ████████╗███████╗██╗██╗     ███████╗███████╗
#   ██╔══██╗██╔═══██╗╚══██╔══╝██╔════╝██║██║     ██╔════╝██╔════╝
#   ██║  ██║██║   ██║   ██║   █████╗  ██║██║     █████╗  ███████╗
#   ██║  ██║██║   ██║   ██║   ██╔══╝  ██║██║     ██╔══╝  ╚════██║
#   ██████╔╝╚██████╔╝   ██║   ██║     ██║███████╗███████╗███████║
#   ╚═════╝  ╚═════╝    ╚═╝   ╚═╝     ╚═╝╚══════╝╚══════╝╚══════╝
#
#
# source https://github.com/luiznux/dotfiles
# This is the install scrpit that resolves most of the files in this repository.
# It will install some packages too and only works in Arch linux distro.
# After you run it, a 'install.log' file will be created, in case of erros see it.
# If some bugs or issues happen, let me know about it.
# Do you have any sugestions and/or comments? Just call me.
#----------------------------------------------------------------------------------

#Variables
dotfiles=$(pwd)
AUR=~/AUR
errors=0

#func to break line with echo command
break_line(){
    echo ""
}

#write some installings  process in the archive 'install.log'
log(){
    $*
    $* >> $dotfiles/install.log
}

#this func will only write on the archive 'install.log' if a bash error occurred
log_error(){
    $* 2>> $dotfiles/install.log
}

#print erro mgs
erro_msg(){
    ((++errors)) \
    && echo "  ERROR[$[errors]]" && break_line
}

#exit dir func
exit_dir(){
    cd ..
}

#func to install aur packages
make_pkg_AUR(){
    cd $AUR && git clone https://aur.archlinux.org/$1.git && cd $1 && makepkg -i --noconfirm && exit_dir
}

####func to setup my directory tree
dir_tree(){

    log echo "#----------------------------------------------- Setup directory tree"
    mkdir -vp ~/{Github/{luiznux,prog,other},AUR,Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,Pictures/{Screenshots},Documents,Desktop,projects,.vim,.config/{i3,polybar,ranger}} \
    && log echo "        Directory tree {OK}" && break_line || log erro_msg
}

####func to install packages on arch linux
install_packages(){

    log echo "#----------------------------------------------- Packages"
    log echo "     Installing packages"
    log_error sudo pacman -Sy xorg xclip man gvim tree neofetch firefox rxvt-unicode rxvt-unicode-terminfo urxvt-perls  cmake libmpdclient wget i3-gaps i3lock-color ranger w3m nemo nemo-fileroller papirus-icon-theme sl feh vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts clang i7z cpupower alsa alsa-utils alsa-firmware calcurse pulseaudio ttf-font-awesome libxss libcurl-gnutls dmenu mailutils llvm dhcp dhcpcd haveged xreader calibre ristretto tumbler evince playerctl check gobject-introspection transmission-gtk file ffmpegthumbnailer highlight atool imagemagick fftw openjdk11-src lxrandr-gtk3 mtpfs gvfs-mtp gvfs-gphoto2 android-file-transfer libmtp ufw sxiv yasm lxappearance gtk-chtheme xorg-xinit intltool dbus-glib gnome-shell gnome-session yelp-tools docbook-xsl go clisp cmatrix mlocate dunst cargo discord zenity --noconfirm \
    && log echo "        Packages {OK}" && brek_line || log erro_msg
}

####func to install some python packages
Python_config(){
    log echo "#----------------------------------------------- PYTHON CONFIG" && break_line
    log_error sudo pacman -S python-pip python-sphinx python-dbus python2-gobject  python-psutil python-urwid --noconfirm \
    && pip install powerline-shell \
    && log echo "	     Python {OK}" && break_line || log erro_msg
}

####func to install the graphic drivers(depends of your hardware)
Graphic_drivers(){

    log echo "#----------------------------------------------- Graphic drives and NVIDIA" && break_line
    log_error sudo pacman -S xf86-video-intel vulkan-intel mesa-demos nvidia nvidia-utils nvidia-settings bumblebee --noconfirm \
    && log echo "	     Graphic Drivers {OK}" && break_line || log erro_msg
}

####AUR Packges installation func(with MAKEPKG)
AUR_install(){

    log echo "#---------------------------------------- AUR packages" && break_line
    log echo "Installing some AUR Packages" && break_line
    log echo "#OPTIMUS MANAGER AND GDM" && break_line
    log echo " gdm-prime optimus-manager optimus-manager-qt"
    log_error make_pkg_AUR gdm-prime \
    && log_error make_pkg_AUR optimus-manager \
    && log_error make_pkg_AUR optimus-manager-qt \
    && log echo "Done" && break_line || log erro_msg

    log echo "#------------ Other packages" && break_line
    log echo "nvidia-xrun-pm python-pdftotext polybar thermald ttf-weather-icon wps-office.git "
    log echo "ttf-wps-fonts qdirstat jmtpfs sublime-text-dev speedometer cli-visualizer spotify " && break_line
    log_error make_pkg_AUR nvidia-xrun-pm \
    && log_error gpg --keyserver keyserver.ubuntu.com --recv-keys 4773BD5E130D1D45 && log_error make_pkg_AUR spotify \
    && log_error make_pkg_AUR ffmpeg-compat-57 \
    && log_error make_pkg_AUR python-pdftotext \
    && log_error make_pkg_AUR polybar.git \
    && log_error make_pkg_AUR thermald.git \
    && log_error make_pkg_AUR ttf-weather-icons.git \
    && log_error make_pkg_AUR wps-office \
    && log_error make_pkg_AUR ttf-wps-fonts \
    && log_error make_pkg_AUR qdirstat \
    && log_error make_pkg_AUR jmtpfs \
    && log_error make_pkg_AUR sublime-text-dev \
    && log_error make_pkg_AUR speedometer \
    && log_error make_pkg_AUR cli-visualizer \
    && log echo " AUR pkgs Done" && break_line || log erro_msg
}

####Emacs install and copy my config file
emacs(){

    log echo "#---------------------------------------- EMACS INSTALL" && break_line
    log_error cd $dotfiles && cp -r emacs/.emacs.d  ~/.emacs.d/ \
    && log echo "     Emacs config {OK} " && break_line || log erro_msg

    log_error cd ~/ && log_error wget gnu.c3sl.ufpr.br/ftp/emacs/emacs-26.3.tar.xz && log_error tar -xvf emacs-26.3.tar.xz && rm emacs-26.3.tar.xz \
    && log_error cd ~/emacs-26.3 && log_error ./autogen.sh && log_error ./configure && log_error make && log_error sudo make install \
    && log echo "     Emacs  Install  {OK}" && break_line || log erro_msg
}


####move all the others dotfiles
general_config(){

    log echo "#---------------------------------------- Setup i3 and polybar" && break_line
    cd $dotfiles && cp i3/config ~/.config/i3/ \
    && cd $dotfiles && cp -r polybar/*  ~/.config/polybar/ \
    && log echo "     I3 and Polybar config {OK} " && break_line || log erro_msg

    #log echo "#---------------------------------------- Polyabar Forecast" && break_line
    #log_error cd ~/.config/polybar/ && git clone https://github.com/kamek-pf/polybar-forecast.git \
    #&& log_error cd ~/.config/polybar/polybar-forecast/ && log_error cargo build --release \
    #&& cp $dotfiles/polybar/config.toml .config/polybar/polybar-forecast/ \
    #&& log echo"	  Scripts {OK}" && break_line || log erro_msg

    log echo "#---------------------------------------- Ranger config" && break_line
    cd $dotfiles && cp config/rc.conf  ~/.config/ranger/ \
    && log echo "     Ranger config file setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Vim config setup" && break_line
    cd $dotfiles && cp vim/.vimrc ~/.vimrc \
    && cd $dotfiles && cp -r vim/.vim/ ~/ \
    && log echo "     Vim setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup font" && break_line
    sudo mkdir -p /usr/share/fonts/ \
    && cd $dotfiles && sudo cp -R config/fonts/source-code-pro /usr/local/share/fonts/ \
    && log echo "     Fount setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Locale" && break_line
    log_error cd $dotfiles && sudo cp config/locale.conf  /etc/ && log sudo locale-gen \
    && log echo "     Locale setup {OK}" && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Xresources" && break_line
    cd $dotfiles && cp config/.Xresources ~/.Xresources \
    && log echo "     Xresources setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup gitignore global file" && break_line
    cd $dotfiles && cp config/.gitignore_global  ~/ \
    && log echo "     Gitignore global setup {OK} " && break_line || log erro_msg
    cd $dotfiles && cp config/.gitconfig ~/ \
    && log echo "     Gitconfig setup {OK} " && break_line || log erro_msg && ((errors+=1))

    log echo "#---------------------------------------- Setup background image" && break_line
    cd $dotfiles && cp config/morpho.jpg ~/.config/wallpaper.jpg  \
    && log echo "     Wallppaer setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Themes" && break_line
    cd $dotfiles && cd config/ && cp -r gtk-2.0 gtk-3.0 ~/.config \
    && cd $dotfiles && cd config/ && cp .gtkrc-2.0 ~/.gtkrc-2.0 \
    && log echo "     GTK themes setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Pacman config" && break_line
    cd $dotfiles && log_error sudo cp config/pacman/mirrorlist /etc/pacman.d/ \
    && sudo rm /etc/pacman.conf && cd $dotfiles && sudo cp config/pacman/pacman.conf  /etc/ \
    && log echo "     Pacman config {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Other Configs " && break_line
    cd $dotfiles && sudo cp config/urxvt-resize-font/resize-font /usr/lib64/urxvt/perl/ &&  sudo chmod +x /usr/lib64/urxvt/perl/resize-font \
    && cd $dotfiles && cp config/screenshots.sh ~/.config/ \
    && cd $dotfiles && cp -r config/sxiv ~/.config/ \
    && cd $dotfiles && cp config/.bashrc ~/ \
    && cd $dotfiles && cp -r config/dunst ~/.config/ \
    && cd $dotfiles && cp -r config/powerline-shell/ ~/.config/ \
    && cd $dotfiles && sudo cp config/X11/xinit/xinitrc /etc/X11/xinit/ \
    && log echo " General config {OK}" && break_line || log error_msg
}


####func that install laptoptools
laptop_config(){

    log echo "Do you want install laptop configs ?(answer with y or n) ->" && read $option

    if [$option -eq "y"]; then
        log echo "#----------------------------------------- Laptop config" && break_line
        log echo "#--------- Laptop packges" && break_line
        log_error sudo pacman -S acpi tlp libinput xf86-input-synaptics xorg-xinput powertop xfce4-power-manager bluez bluez-utils bbswitch --noconfirm \
        && log echo " Done" && break_line || log erro_msg

        log echo "#--------- BUMBLEBEE CONFIG (LAPTOP ONLY)" && break_line
        log sudo mkdir -vp /etc/modprobe.d/ && log sudo mkdir -vp /proc/acpi/
        log_error sudo gpasswd -a luiznux bumblebee \
        && cd $dotfiles && log_error sudo cp config/bbswitch.conf /etc/modprobe.d/bbswitch.conf \
        && log_error tee /proc/acpi/bbswitch <<<OFF \
        && log_error sudo systemctl enable bumblebeed.service \
        && log echo "     Bumblebee {OK}" && break_line || log erro_msg

        log echo "#--------- Light(brithness control)" && break_line
        cd ~/Github/prog/ && log_error git clone https://github.com/haikarainen/light \
        && cd ~/Github/prog/light && log_error ./autogen.sh && log_error ./configure && sudo make \
        && log echo "     Light {OK}" && break_line || log erro_msg

        #Batterymon and depence
        log_error make_pkg_AUR makepython2-distutils-extra \
        && log_error make_pkg_AUR batterymon-clone \
        && cd $dotfiles && sudo cp config/tlp.conf /etc/tlp.conf \
        && log echo "     Laptop configs {OK}" && break_line || log erro_msg

    elif [$option -eq "n"]; then
        log echo "#------------------------------------ Laptop config {SKIPED}"
    fi
}

####func that enable some services
systemd_init(){

    log echo "#---------------------------------------- ENABLE SYSTEMCTL SERVICES" && break_line
    log_error sudo systemctl enable NetworkManager.service \
    && log_error sudo systemctl enable gdm.service \
    && log_error sudo systemctl enable tlp.service \
    && log_error sudo systemctl enable ufw.service && log_error ufw enable \
    && log_error sudo systemctl enable optimus-manager.service \
    && log echo "Done" && break_line || log erro_msg
}

#######################MAIN
dir_tree
install_packages
Python_config
Graphic_drivers
AUR_install
emacs
general_config
laptop_config
#systemd_init

log echo "------------- END OF INSTALL ------------" && break_line
log echo " [$[errors]]Errors reported, see 'install.log' for more details" && break_line
