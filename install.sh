#!/bin/bash
#
#     ██╗███╗   ██╗███████╗████████╗ █████╗ ██╗     ██╗
#     ██║████╗  ██║██╔════╝╚══██╔══╝██╔══██╗██║     ██║
#     ██║██╔██╗ ██║███████╗   ██║   ███████║██║     ██║
#     ██║██║╚██╗██║╚════██║   ██║   ██╔══██║██║     ██║
#     ██║██║ ╚████║███████║   ██║   ██║  ██║███████╗███████╗
#     ╚═╝╚═╝  ╚═══╝╚══════╝   ╚═╝   ╚═╝  ╚═╝╚══════╝╚══════╝
#
#
# source https://github.com/luiznux/dotfiles
# This is the install scrpit that resolves most of the files in this repository.
# It will install some packages too and only works in Arch linux distro.
# After you run it, a 'install.log' file will be created, in case of erros see it.
# If some bugs or issues happen, let me know about it.
# Do you have any sugestions and/or comments? Just call me.
#----------------------------------------------------------------------------------

#### Variables
dotfiles=$(pwd)
AUR=~/AUR
GIT=~/Github
errors=0

#### break line with echo command
break_line(){
    echo $'\n\n'
}

#### delete old log file
clean_log(){
    rm -f $dotfiles/install.log
}

#### write some installings  process in the archive 'install.log'
log(){
    $*
    $* >> $dotfiles/install.log
}

# this func will only write on the archive 'install.log' if a bash error occurred
log_error(){
    $* 2>> $dotfiles/install.log
}

#### print erro mgs
erro_msg(){
    ((errors+=1)) && echo "  ERROR[$[errors]]" && break_line
}

#### exit dir func
exit_dir(){
    cd ..
}

#### install aur packages
make_pkg_AUR(){
    #in case the dir already exists
    if [ -d "$AUR/$1" ];then
        cd $AUR/$1 && makepkg -is --noconfirm && exit_dir

    else
        cd $AUR && git clone https://aur.archlinux.org/$1.git && cd $1 && makepkg -is --noconfirm && exit_dir
    fi
}

#### clean AUR dir
clean_AUR(){
    rm -rf $AUR/*
}

#### remove old log files
clean_log(){
    cd $dotfiles && rm -f install.log && echo "cleaned old log files" || break_line
}


#### setup my directory tree
dir_tree(){
    log echo "#----------------------------------------------- Setup directory tree"
    mkdir -vp ~/{Github/{luiznux,prog,other},AUR,Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,Pictures/Screenshots,Documents,Desktop,projects,.vim,.config/{i3,polybar,ranger}} \
    && log echo "        Directory tree {OK}" && break_line || log erro_msg
}


#### install packages on arch linux
install_packages(){
    log echo "#----------------------------------------------- Packages"
    log echo "     Installing packages"
    log_error sudo pacman -Sy xorg xclip ufw man tree colordiff zsh zsh-completions neofetch firefox rxvt-unicode rxvt-unicode-terminfo urxvt-perls cmake libmpdclient wget i3-gaps ranger w3m nemo nemo-fileroller papirus-icon-theme sl feh vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts clang ccls i7z cpupower alsa alsa-utils alsa-firmware calcurse pulseaudio ttf-font-awesome libxss libcurl-gnutls dmenu mailutils llvm dhcp dhcpcd haveged xreader calibre ristretto eog tumbler evince playerctl check gobject-introspection transmission-gtk file ffmpegthumbnailer highlight atool imagemagick fftw openjdk11-src lxrandr-gtk3 mtpfs gvfs-mtp gvfs-gphoto2 android-file-transfer libmtp ufw sxiv yasm lxappearance gtk-chtheme xorg-xinit intltool dbus-glib gnome-shell gnome-session yelp-tools docbook-xsl go clisp cmatrix mlocate dunst cargo discord zenity scrot paprefs pavucontrol code youtube-dl qt gimp picom cups cups-pdf system-config-printer gdm pandoc texlive-most rofi gnome-keyring nmap deepin-screenshot ntp bash-language-server --noconfirm \
    && log echo "        Packages {OK}" && break_line || log erro_msg
}


#### to install some python packages
Python_config(){
    log echo "#----------------------------------------------- PYTHON CONFIG" && break_line
    log_error sudo pacman -S python-pip python-sphinx python-dbus python2-gobject  python-psutil python-urwid python-pywal --noconfirm \
    && log echo "	     Python {OK}" && break_line || log erro_msg
}


#### install the graphic drivers(depends of your hardware)
Graphic_drivers(){
    log echo "#----------------------------------------------- Graphic drives and NVIDIA" && break_line
    log_error sudo pacman -S xf86-video-intel vulkan-intel mesa-demos nvidia nvidia-utils nvidia-settings --noconfirm \
    && log echo "	     Graphic Drivers {OK}" && break_line || log erro_msg
}


#### AUR Packges installation func(with MAKEPKG)
AUR_install(){
    log echo "#---------------------------------------- AUR packages" && break_line
    log echo "Installing some AUR Packages" && break_line
    log echo "python-pdftotext polybar thermald ttf-weather-icon rar pygtk python2-twodict-git youtube-dl-gui-git jetbrains-toolbox "
    log echo "wps-office ttf-wps-fonts qdirstat jmtpfs sublime-text-dev speedometer cli-visualizer spotify spicetify-cli" && break_line
    log_error make_pkg_AUR python-pdftotext \
    && log_error make_pkg_AUR polybar \
    && log_error make_pkg_AUR i3lock-color-git \
    && log_error make_pkg_AUR thermald \
    && log_error make_pkg_AUR ttf-weather-icons \
    && log_error make_pkg_AUR qdirstat \
    && log_error make_pkg_AUR jmtpfs \
    && log_error make_pkg_AUR sublime-text-dev \
    && log_error make_pkg_AUR speedometer \
    && log_error make_pkg_AUR cli-visualizer \
    && log_error make_pkg_AUR mailspring \
    && log_error make_pkg_AUR pygtk \
    && log_error make_pkg_AUR rar \
    && log_error make_pkg_AUR python-ueberzug \
    && log_error make_pkg_AUR python2-twodict-git \
    && log_error make_pkg_AUR youtube-dl-gui-git \
    && log_error make_pkg_AUR jetbrains-toolbox \
    && log_error make_pkg_AUR spicetify-themes-git \
    && log_error make_pkg_AUR ttf-wps-fonts \
    && log_error make_pkg_AUR wps-office \
    && log_error make_pkg_AUR wps-office-mui \
    && log echo "--------- AUR pkgs Done " && break_line || log erro_msg
    break_line
}


#### Emacs install and copy my config file
emacs(){
    log echo "#---------------------------------------- EMACS INSTALL" && break_line
    log_error cd $dotfiles && cp -r emacs/.emacs.d  ~/.emacs.d/ \
    && log echo "     Emacs config {OK} " && break_line || log erro_msg

    log_error cd ~/ && log_error wget gnu.c3sl.ufpr.br/ftp/emacs/emacs-26.3.tar.xz && log_error tar -xvf emacs-26.3.tar.xz && rm emacs-26.3.tar.xz \
    && log_error cd ~/emacs-26.3 && log_error ./autogen.sh && log_error ./configure && log_error make && log_error sudo make install \
    && log echo "     Emacs  Install  {OK}" && break_line || log erro_msg
}

#### Config my dropbox sync folder
dropbox_setup(){

    log echo "#---------------------------------------- Setup Dropbox Packages " && break_line
    #in case the dir already exists
    if [ -d "$AUR/dropbox" ];then
        log echo " directory already exists and its not empty !" && break_line

    else
        log_error cd $AUR && log_error git clone https://aur.archlinux.org/dropbox.git && exit_dir
    fi

    log_error cd $AUR/dropbox && log_error wget https://linux.dropbox.com/fedora/rpm-public-key.asc \
    && log_error gpg --import rpm-public-key.asc && break_line && log echo " Key imported" \
    && log_error make_pkg_AUR dropbox \
    && log_error make_pkg_AUR nemo-dropbox \
    && log echo "     Dropbox install packages {OK}" && break_line || log erro_msg
}

#### move all the others dotfiles
general_config(){
    log echo "#---------------------------------------- Setup i3 and polybar" && break_line
    cd $dotfiles && cp i3/config ~/.config/i3/ \
    && cd $dotfiles && cp -r polybar/*  ~/.config/polybar/ \
    && log echo "     I3 and Polybar config {OK} " && break_line || log erro_msg

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
    cd $dotfiles && cp config/wallpapers/morpho.jpg  ~/.config/wallpaper.jpg  \
    && log echo "     Wallppaer setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Themes" && break_line
    cd $dotfiles/themes/gtk/ && tar -xvf Sweet-Dark.tar.xz  && sudo mv Sweet-Dark /usr/share/themes/
    cd $dotfiles && cd config/gtk/ && cp -r gtk-2.0 gtk-3.0 ~/.config \
    && cd $dotfiles && cd config/gtk/ && cp .gtkrc-2.0 ~/.gtkrc-2.0 \
    && log echo "     GTK themes setup {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Pacman config" && break_line
    cd $dotfiles && log_error sudo cp config/pacman/mirrorlist /etc/pacman.d/ \
    && sudo rm /etc/pacman.conf && cd $dotfiles && sudo cp config/pacman/pacman.conf  /etc/ \
    && log echo "     Pacman config {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Setup Zsh" && break_line
    log sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" \
    && cp $dotfiles/config/.zshrc ~/ \
    && cd $GIT/prog/ && git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
    && log echo "     Zsh config {OK} " && break_line || log erro_msg

    log echo "#---------------------------------------- Other Configs " && break_line
    cd $dotfiles && sudo cp config/urxvt/urxvt-resize-font/resize-font /usr/lib64/urxvt/perl/ &&  sudo chmod +x /usr/lib64/urxvt/perl/resize-font \
    && gsettings set org.cinnamon.desktop.default-applications.terminal exec st \
    && cd $GIT/prog && git clone https://github.com/luiznux/st && cd st/ && make && sudo make clean install \
    && cd $GIT/luiznux && git clone https://github.com/luiznux/org.git && ln -s $GIT/luiznux/org ~/org && exit_dir \
    && cd $GIT/luiznux && git clone https://github.com/luiznux/codes.git && ln -s $GIT/luiznux/codes ~/projects/ && exit_dir \
    && cd $dotfiles && cp config/scripts/screenshots.sh  ~/.config/ \
    && cd $dotfiles && sudo cp config/scripts/{ca,simple-push,volume} /usr/local/bin/ \
    && cd $dotfiles && cp -r config/sxiv ~/.config/ \
    && cd $dotfiles && cp config/.bashrc ~/ \
    && cd $dotfiles && cp -r config/dunst ~/.config/ \
    && cd $dotfiles && sudo cp config/X11/xinit/xinitrc /etc/X11/xinit/ \
    && cd $dotfiles && sudo cp config/X11/xorg.conf.d/* /etc/X11/xorg.conf.d/\
    && log echo " General config {OK}" && break_line || log error_msg
}


### to clone some repositories
git_repository_setup(){
    log echo "#---------------------------------------- Git Repositories Clone " && break_line
    cd $GIT/other && git clone https://github.com/stark/Color-Scripts.git && exit_dir
    cd $GIT/other && git clone https://github.com/morpheusthewhite/spicetify-themes.git && exit_dir
    cd $GIT/other && git clone https://github.com/PlusInsta/discord-plus && exit_dir
    cd $GIT/other && curl -O https://raw.githubusercontent.com/bb010g/betterdiscordctl/master/betterdiscordctl \
        && chmod +x betterdiscordctl && sudo mv betterdiscordctl /usr/local/bin && sudo betterdiscordctl upgrade
    cd $GIT/other && git clone https://github.com/sebastiencs/icons-in-terminal.git && exit_dir
    cd $GIT/other && git clone https://github.com/Brettm12345/github-moonlight  && exit_dir
    log echo " Git rep  Done" && break_line
}


#### to install laptoptools
laptop_config(){
    log echo "Do you want install laptop configs ?(answer with y or n)"
    log read -p "-->" option

    if [ $option == "y" ]; then
        log echo "#----------------------------------------- Laptop config" && break_line
        log echo "#--------- Laptop packges" && break_line
        log_error sudo pacman -S acpi tlp bumblebee xf86-input-synaptics xfce4-power-manager light bluez-utils --noconfirm \
        && log_error make_pkg_AUR nvidia-xrun \
        && log echo " Lapto packages {OK}" && break_line || log erro_msg

        log echo "#----------------------------------------- Optimus Manager and Gdm prime(AUR)" && break_line
        log_error cd $AUR && git clone https://aur.archlinux.org/gdm-prime.git && cd gdm-prime && makepkg -is && exit_dir \
        && log_error make_pkg_AUR optimus-manager \
        && log_error make_pkg_AUR optimus-manager-qt \
        && log echo " Optimus  {OK}" && break_line || log erro_msg

        log echo "#--------- Bbswitch CONFIG (LAPTOP ONLY)" && break_line
        log sudo mkdir -vp /etc/modprobe.d/ && log sudo mkdir -vp /proc/acpi/
        log_error sudo gpasswd -a luiznux bumblebee \
        && cd $dotfiles && log_error sudo cp config/bbswitch.conf /etc/modprobe.d/bbswitch.conf \
        && sudo touch /proc/acpi/bbswitch \
        && log_error sudo tee /proc/acpi/bbswitch <<<OFF \
        && log echo "     Bbswitch {OK}" && break_line || log erro_msg

        log echo "#--------- Light(brithness control)" && break_line
        cd ~/Github/prog/ && log_error git clone https://github.com/haikarainen/light \
        && cd ~/Github/prog/light && log_error ./autogen.sh && log_error ./configure && sudo make \
        && log echo "     Light {OK}" && break_line || log erro_msg

        #TLP config
        cd $dotfiles && sudo cp config/tlp.conf /etc/tlp.conf \
        && log echo "     Laptop configs {OK}" && break_line || log erro_msg

        log echo "Systemctl for laptop services"
        log_error systemctl enable tlp.service optimus-manager.service bumblebeed.service \
        && log_error systemctl disable bluetooth.service \

    else
        log echo "#------------------------------------ Laptop config {SKIPED}"
        break_line
    fi
}


#### run nvidia xconfig
nvidia_xorg_config(){
    log echo "Do you want run nvidia-xconfig to generate a xconfig file ? (answer with y or n)"
    log echo "Only answer 'y' if you are using nvidia graphic card and have the drivers"
    log read -p "-->" option

    if [ $option == "y" ]; then
        sudo nvidia-xconfig
        echo "Done!" && break_line
    else
        echo "Nvidia xconfig {SKIPED} " && break_line
    fi
}


#### enable some services
systemd_init(){
    log echo "#---------------------------------------- ENABLE SYSTEMCTL SERVICES" && break_line
    log_error systemctl enable gdm.service ufw.service ntpd.service \
    && log_error sudo ufw enable \
    && log echo "Done" && break_line || log erro_msg
}


####################### MAIN
clean_log
dir_tree
install_packages
Python_config
Graphic_drivers
AUR_install
emacs
dropbox_setup
general_config
git_repository_setup
laptop_config
nvidia_xorg_config
systemd_init

log echo "------------- END OF INSTALL ------------" && break_line
log echo " [$[errors]] Errors reported, see 'install.log' for more details" && break_line
