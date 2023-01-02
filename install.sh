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
errors=0

#### break line with echo command
break_line(){
    echo $'\n\n'
}

#### delete old log file
clean_log(){
    rm -f "$dotfiles"/install.log
}

#### write some installings  process in the archive 'install.log'
log(){
    "$*"
    "$*" >> "$dotfiles"/install.log
}

#### this func will only write on the archive 'install.log' if a bash error occurred
log_error(){
    "$*" 2>> "$dotfiles"/install.log
}

#### print erro mgs
erro_msg(){
    ((errors+=1)) && echo "  ERROR[$(errors)]" && break_line
}

#### exit dir
exit_dir(){
    cd ..
}

#### install aur packages
make_pkg_AUR(){
    #in case the dir already exists
    if [ -d "$AUR/$1" ];then
        cd $AUR/"$1" && makepkg -csi --noconfirm && exit_dir
    else
        cd $AUR && git clone https://aur.archlinux.org/"$1".git && cd "$1" && makepkg -csi --noconfirm && exit_dir
    fi
}

#### setup my directory tree
dir_tree(){
    log echo "#----------------------------------------------- Setup directory tree"
    mkdir -vp ~/{AUR,Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,Pictures/Screenshots,Documents,Desktop,sandbox,projects/luiznux,.vim,.config/{i3,polybar,ranger,rofi,alacritty,scripts,picom}} \
        && log echo "        Directory tree {OK}" && break_line || log erro_msg
}

#### install packages on arch linux
install_packages(){
    log echo "#----------------------------------------------- Packages"
    log echo "     Installing packages"

    essencials=" xorg xclip cmake libxss llvm xorg-xinit "

    linux_gadgets=" man tree colordiff exa fzf wget check file highlight atool mlocate ripgrep nmap ntp ncdu haveged dmidecode hwdetect "

    utilities=" htop atop calcurse cpupower dmenu rofi cmatrix neofetch ranger sl youtube-dl pacmanlogviewer expac xfce4-settings lxinput hddtemp lm_sensors xsensors psensor gedit baobab s-tui smartmontools usbutils "

    program_languages=" global ctags clang bear ccls go gopls gobject-introspection bash-language-server clisp cargo openjdk11-src shellcheck clojure leiningen nodejs m17n-lib "

    graphic=" i3-wm lxrandr-gtk3 arandr qt zenity dunst picom lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings lightdm-slick-greeter "

    file_open=" nemo nemo-fileroller i7z xreader calibre evince pandoc texlive-most "

    themes=" papirus-icon-theme lxappearance gtk-chtheme gpick "

    font=" adobe-source-code-pro-fonts ttf-sourcecodepro-nerd noto-fonts-cjk noto-fonts-emoji noto-fonts ttf-font-awesome gnome-font-viewer "

    gnome=" intltool dbus-glib gnome-shell gnome-session yelp-tools docbook-xsl gnome-system-monitor gnome-control-center gnome-calculator gnome-calendar gnome-characters gnome-power-manager "

    # with pipewire packages
    audio=" alsa alsa-utils alsa-firmware alsa-plugins pipewire pipewire-docs pipewire-alsa pipewire-pulse pipewire-jack wireplumber wireplumber-docs pavucontrol libmpdclient fftw playerctl vlc audacity gnome-music easyeffects vvave rhythmbox "
    #audio=" alsa alsa-utils alsa-firmware alsa-plugins pulseaudio pulseaudio-alsa pavucontrol libmpdclient fftw playerctl vlc paprefs "

    image=" eog feh tumbler gthumb ffmpegthumbnailer webp-pixbuf-loader mplayer gst-libav imagemagick sxiv gimp scrot flameshot w3m ueberzug gnome-video-effects cheese pdf2svg "

    android_device=" mtpfs gvfs-mtp gvfs-gphoto2 android-file-transfer libmtp yasm "

    gnu_things=" libcurl-gnutls mailutils "

    term_shell=" zsh zsh-completions libvterm alacritty rxvt-unicode rxvt-unicode-terminfo urxvt-perls "

    printer=" cups cups-pdf system-config-printer "

    security=" ufw gnome-keyring keychain seahorse "

    network=" dhcp dhcpcd "

    browsers=" firefox chromium torbrowser-launcher "

    python=" python-pip python-sphinx dbus-python python-psutil python-urwid python-pywal python-pdftotext python-mutagen "

    others=" transmission-gtk gparted discord bleachbit kdenlive mesa-demos ispell aspell aspell-pt aspell-en cowsay "

    log_error sudo pacman -Syu --noconfirm --needed \
              "$essencials" \
              "$linux_gadgets" \
              "$utilities" \
              "$program_languages" \
              "$graphic" \
              "$file_open" \
              "$themes" \
              "$font" \
              "$gnome" \
              "$audio" \
              "$image" \
              "$android_device" \
              "$gnu_things" \
              "$term_shell" \
              "$printer" \
              "$security" \
              "$network" \
              "$browsers" \
              "$python" \
              "$others" \
        && log echo "        Packages {OK}" && break_line || log erro_msg
}

#### install the graphic drivers(depends of your hardware)
Graphic_drivers(){
    log echo "#----------------------------------------------- Graphic drives" && break_line
    if [ "$GPU" == "1" ]; then
        log_error sudo pacman -Suy xf86-video-intel vulkan-intel --noconfirm --needed

    elif [ "$GPU" == "2" ]; then
        log_error sudo pacman -Suy nvidia nvidia-utils nvidia-settings --noconfirm --needed

    elif [ "$GPU" == "3" ]; then
        log_error sudo pacman -Suy xf86-video-amdgpu vulkan-radeon --noconfirm --needed

    else
        log echo "GPU Packages skipped " && break_line || log erro_msg
    fi

    log echo "	     Graphic Drivers {OK}" && break_line || log erro_msg
}

#### AUR Packges installation func(with MAKEPKG and others with yay)
AUR_install(){
    log echo "#---------------------------------------- AUR packages" && break_line

    log echo "-------------------------------- Installing yay package" && break_line
    log_error make_pkg_AUR yay \
    && log echo "----------------------------- YAY Installed!" && break_line || log erro_msg

    log echo "----------------------------- Installing AUR General packages" && break_line || log erro_msg
    packages=" polybar archlinux-artwork i3lock-color-git autotiling nwg-launchers thermald mictray qdirstat jmtpfs zscroll-git clojure-lsp-bin speedometer cli-visualizer rar mon2cam-git fancontrol-gui "
    fonts=" ttf-weather-icons "
    themes=" catppuccin-gtk-theme themix-full-git "
    wps_office=" ttf-wps-fonts wps-office-mui-pt-br wps-office "

    log_error yay -Syu \
              "$packages" \
              "$fonts" \
              "$themes" \
              "$wps_office" \
              --noconfirm --needed --nocleanmenu --nodiffmenu \
        && log echo "----------------------------- AUR General packages  Done " && break_line || log erro_msg
}

# If you using ryzen cpu, see https://wiki.archlinux.org/title/Ryzen
AMD_CPU(){
    if [ "$amd_option" == "y" ]; then
        log echo "#----------------------------------------- Installing AMD CPU packages"
        log_error sudo pacman -Syu amd-ucode --noconfirm --needed \
            && log_error yay -Syu it87-dkms-git zenmonitor3-git zenpower3-dkms --noconfirm --needed --nocleanmenu --nodiffmenu \
            && log echo "#-------------------------------------- AMD packages {OK}" && break_line || log erro_msg
    else
        log echo "#------------------------------------ AMD packages {SKIPED}" && break_line
        break_line
    fi
}

#TODO: tratar exceçao quando arquivo ja existe
#### Setup my custom emacs config, also clone the master branch from
#### gnu savannah, build and install
emacs(){
    log echo "#---------------------------------------- EMACS Config" && break_line
    log_error cd ~/ && git clone https://github.com/luiznux/emacs.d.git git-emacs \
        && mv  git-emacs/.emacs.d ./ && rm -rf git-emacs \
        && log echo "     Emacs config {OK} " && break_line || log erro_msg

    log_error cd ~/ && log_error git clone https://git.savannah.gnu.org/git/emacs.git \
    && log echo "     Emacs savannah cloned  {OK}" && break_line || log erro_msg
}

#### Install some lsp servers packages(using npm)
emacs_lsp_packages(){
    log echo "#---------------------------------------- EMACS LSP Packages INSTALL" && break_line
    log echo "Installing using npm the following packages:"
    log echo "html-lsp, css-lsp, json-lsp yaml-lsp, dockerfile-lsp, bash-lsp and vim-lsp sql-lsp" && break_line

    log_error npm install -g vscode-html-languageserver-bin \
              vscode-css-languageserver-bin \
              vscode-json-languageserver \
              yaml-language-server \
              dockerfile-language-server-nodejs \
              bash-language-server \
              vim-language-server \
              sql-language-server \
    && log echo "     Emacs LSP Packages config {OK} " && break_line || log erro_msg
}

#TODO: tratar exceçao quando arquivo ja existe
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
    && log_error yay -Syu  dropbox nemo-dropbox --noconfirm --needed --nocleanmenu --nodiffmenu \
    && log echo "     Dropbox install packages {OK}" && break_line || log erro_msg
}

#### I3 and Polybar config
i3_polybar_setup(){
    log echo "#---------------------------------------- Setup i3 and polybar" && break_line
    cd "$dotfiles" && cp -r config/i3 ~/.config/i3 \
        && cp -r config/polybar  ~/.config/polybar \
        && log echo "     i3 and Polybar config {OK} " && break_line || log erro_msg
}

ranger_setup(){
    log echo "#---------------------------------------- Ranger Setup" && break_line
    cd "$dotfiles" && cp config/rc.conf  ~/.config/ranger/ \
    && log echo "     Ranger config file setup {OK} " && break_line || log erro_msg
}

vim_setup(){
    log echo "#---------------------------------------- Vim config setup" && break_line
    cd "$dotfiles" && cp config/vim/.vimrc ~/.vimrc \
        && cp -r config/vim/.vim/ ~/ \
        && log echo "     Vim setup {OK} " && break_line || log erro_msg
}

locale_setup(){
    log echo "#---------------------------------------- Setup Locale" && break_line
    log_error cd "$dotfiles" && sudo cp config/locale.conf  /etc/ && log sudo locale-gen \
    && log echo "     Locale setup {OK}" && break_line || log erro_msg
}

xresources_setup(){
    log echo "#---------------------------------------- Setup Xresources" && break_line
    cd "$dotfiles" && cp config/.Xresources ~/.Xresources \
    && log echo "     Xresources setup {OK} " && break_line || log erro_msg
}

gitignore_setup(){
    log echo "#---------------------------------------- Setup gitignore global file" && break_line
    cd "$dotfiles" && cp config/.gitignore_global  ~/ \
    && log echo "     Gitignore global setup {OK} " && break_line || log erro_msg
    cd "$dotfiles" && cp config/.gitconfig ~/ \
    && log echo "     Gitconfig setup {OK} " && break_line || log erro_msg
}

background_img_setup(){
    log echo "#---------------------------------------- Setup background image" && break_line
    sudo mkdir -p /usr/share/backgrounds/luiznux \
    && cd "$dotfiles" && cp config/wallpapers/morpho.jpg  ~/.config/wallpaper.jpg  \
    && sudo cp -r "$dotfiles"/config/wallpapers/* /usr/share/backgrounds/luiznux/ \
    && log echo "     Wallppaer setup {OK} " && break_line || log erro_msg
}

#TODO: tratar exceçao quando arquivo ja existe
#### Setup gtk themes files
theme_setup(){
    log echo "#---------------------------------------- Setup Themes" && break_line
    sudo cp "$dotfiles"/config/lightdm/* /etc/lightdm/ \
    && cd "$dotfiles"/config/gtk/ && cp -r gtk-2.0 gtk-3.0 ~/.config \
    && cp .gtkrc-2.0 ~/.gtkrc-2.0 \
    && log echo "     GTK themes setup {OK} " && break_line || log erro_msg
}

pacman_setup(){
    log echo "#---------------------------------------- Setup Pacman config" && break_line
    cd "$dotfiles" && sudo rm /etc/pacman.conf && cd "$dotfiles" && sudo cp config/pacman/pacman.conf  /etc/ \
    && log echo "     Pacman config {OK} " && break_line || log erro_msg
}

#TODO: tratar exceçao quando arquivo ja existe
#### Zsh install and setup config files
zsh_setup(){
    log echo "#---------------------------------------- Setup Zsh" && break_line
    log sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended \
    && cp "$dotfiles"/config/.zshrc ~/ \
    && git clone https://github.com/zsh-users/zsh-autosuggestions "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}"/plugins/zsh-autosuggestions \
    && git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}"/plugins/zsh-syntax-highlighting \
    && git clone https://github.com/joshskidmore/zsh-fzf-history-search "${ZSH_CUSTOM:=~/.oh-my-zsh/custom}"/plugins/zsh-fzf-history-search \
    && log echo "     Zsh config {OK} " && break_line || log erro_msg
}

xorg_setup(){
    log echo "#---------------------------------------- Setup Xorg config files" && break_line
    cd "$dotfiles" && sudo cp config/X11/xinit/xinitrc /etc/X11/xinit/ \
    && sudo cp config/X11/xorg.conf.d/* /etc/X11/xorg.conf.d/ \
    && log echo "     Xorg config {OK} " && break_line || log erro_msg
}

urxvt_setup(){
    log echo "#---------------------------------------- URXVT Configs " && break_line
    cd "$dotfiles" && sudo cp config/urxvt/urxvt-resize-font/resize-font /usr/lib64/urxvt/perl/ &&  sudo chmod +x /usr/lib64/urxvt/perl/resize-font \
    && log echo "     URXVT Config {OK} " && break_line || log erro_msg
}

#### move all the others dotfiles
other_config(){
    log echo "#---------------------------------------- Other Configs " && break_line
    cd "$dotfiles" && sudo cp -r config/pipewire/pipewire.conf /etc/pipewire/pipewire.conf \
    && cp -r config/alacritty/alacritty.yml ~/.config/alacritty/ \
    && cp -r config/rofi/* ~/.config/rofi/ \
    && sudo cp config/scripts/{ca,simple-push,volume,nvidia-fan-setup} /usr/local/bin/ \
    && cp -r config/scripts/ ~/.config \
    && cp -r config/picom ~/.config \
    && cp -r config/sxiv ~/.config/ \
    && cp -r config/dunst ~/.config/ \
    && cp -r config/vis ~/.config \
    && cp config/.bashrc ~/ \
    && gsettings set org.cinnamon.desktop.default-applications.terminal exec alacritty \
    && log echo " Other config {OK}" && break_line || log error_msg
}

#### enable some services
systemd_init_core_services(){
    log echo "#---------------------------------------- ENABLE SYSTEMCTL SERVICES" && break_line
    log_error sudo systemctl enable lightdm.service ufw.service ntpd.service cpupower.service \
    && log_error systemctl --user --now disable pipewire-media-session  \
    && log_error systemctl --user --now enable wireplumber \
    && log_error sudo ufw enable \
    && log echo "Done" && break_line || log erro_msg
}

#### install laptoptools
laptop_config(){
    if [ "$laptop_Option" == "y" ]; then
        log echo "#----------------------------------------- Laptop config" && break_line
        log_error sudo pacman -Sy acpi tlp bumblebee xf86-input-synaptics xfce4-power-manager light bluez-utils --noconfirm --needed \
        && log_error make_pkg_AUR nvidia-xrun \
        && log echo "#-------------------------------------- Lapto packages {OK}" && break_line || log erro_msg

        log echo "#----------------------------------------- Optimus Manager and Gdm prime(AUR)" && break_line
        yay -S gdm-prime --noconfirm --nocleanmenu --nodiffmenu \
        && log_error make_pkg_AUR optimus-manager \
        && log_error make_pkg_AUR optimus-manager-qt \
        && log echo "#-------------------------------------- Optimus Manager and Gdm prime {OK}" && break_line || log erro_msg

        log echo "#----------------------------------------- Bbswitch CONFIG (LAPTOP ONLY)" && break_line
        log sudo mkdir -vp /etc/modprobe.d/ && log sudo mkdir -vp /proc/acpi/ \
        && log_error sudo touch /proc/acpi/bbswitch \
        && log_error sudo gpasswd -a "$USER" bumblebee \
        && cd "$dotfiles" && log_error sudo cp config/bbswitch.conf /etc/modprobe.d/bbswitch.conf \
        && log_error sudo tee /proc/acpi/bbswitch <<<OFF \
        && log echo "#-------------------------------------- Bbswitch {OK}" && break_line || log erro_msg

        log echo "#----------------------------------------- TLP CONFIG (LAPTOP ONLY)" && break_line
        cd "$dotfiles" && sudo cp config/tlp.conf /etc/tlp.conf \
        && log echo "#-------------------------------------- TLP config {OK}" && break_line || log erro_msg

        log echo "#----------------------------------------- Systemctl for laptop services"
        log_error sudo systemctl enable tlp.service optimus-manager.service bumblebeed.service \
        && log_error sudo systemctl disable bluetooth.service \
        && log echo "#----------------------------------------- Laptop config DONE"

    else
        log echo "#----------------------------------------- Laptop config {SKIPED}"
        break_line
    fi
}

#### run nvidia xconfig
nvidia_xorg_config(){
    if [ "$nvidia_Option" == "y" ]; then
        log echo "#----------------------------------------- Nvidia Xconfig"
        log sudo nvidia-xconfig
        log echo "#----------------------------------------- Nvidia Xconfig Done!" && break_line
    else
       log echo "#------------------------------------ Nvidia xconfig {SKIPED}" && break_line
    fi
}


####################### MAIN ########################

log echo "Which graphics card will you use?"
log echo -e "1 - INTEL \n2 - NVIDIA \n3 - AMD \n4 - ALL"
read -rp "--> " GPU
break_line

log echo "Are you using a AMD CPU ? (answer with y or n)"
read -rp "--> " amd_option
break_line

log echo "Do you want install laptop configs ?(answer with y or n)"
read -rp "--> " laptop_Option
break_line

log echo "Do you want run nvidia-xconfig to generate a xconfig file ? (answer with y or n)"
log echo "Only answer 'y' if you are using nvidia graphic card"
read -rp "--> " nvidia_Option
break_line


clean_log
dir_tree
install_packages
Graphic_drivers
AUR_install
emacs
emacs_lsp_packages
dropbox_setup
i3_polybar_setup
ranger_setup
vim_setup
locale_setup
xresources_setup
gitignore_setup
background_img_setup
theme_setup
pacman_setup
zsh_setup
xorg_setup
urxvt_setup
other_config
laptop_config
nvidia_xorg_config
systemd_init_core_services


log echo "------------- END OF INSTALL ------------" && break_line
log echo " [$(errors)] Errors reported, see 'install.log' for more details" && break_line
