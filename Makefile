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
#Makefile for my config
#
#
install:

	#ARCH LINUX 
	#setup directory
	mkdir -p ~/{Github/{luiznux,prog},AUR,Torrents,Mangas,Videos,Music,Downloads,Documents,Desktop,projects,.vim,.config/{i3,polybar,ranger}
	cd ~/Github && git init
	@echo "    Directory tree {OK}"

	sudo pacman -S  man vim rxvt-unicode rxvt-unicode-terminfo urxvt-perls cmake python-sphinx libmpdclient wget 3-gaps i3lock ranger w3m xorg xorg-xinit gdm nemo nemo-fileroller sl feh vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts  clang i7z cpupower alsa calcurse  pulseaudio ttf-font-awesome libxss libcurl-gnutls dmenu mailutils

	#graphic drives
	sudo pacman -S xf86-video-intel vulkan-intel

	#AUR packages
	cd ~/AUR && git clone https://aur.archlinux.org/laptop-mode-tools.git && git clone https://aur.archlinux.org/batterymon-clone.git && git clone https://aur.archlinux.org/thermald.git && git clone https://aur.archlinux.org/network-ups-tools.git && git clone https://aur.archlinux.org/tlpui-git.git && git clone https://aur.archlinux.org/polybar.git && https://aur.archlinux.org/gdm3setup-utils.git

	#gtk-config
	sudo pacman -S lxappearance gtk-chtheme xorg-xinput

	#laptop config
	sudo pacman -S acpi libinput xf86-input-synaptics xorg-xinput powertop xfce4-power-manager

	#EMACS INSTALL
	cp -r emacs/.emacs.d  ~/.emacs.d/
	@echo "          Emacs config {OK} "
	wget gnu.c3sl.ufpr.br/ftp/emacs/emacs-26.3.tar.xz && tar -xvf emacs-26.3.tar.xz && mv emacs-26.3/ ~/
	@echo "        Emacs  Install  {OK}"

	#setup i3 and polybar
	cp config/i3/config ~/.config/i3/
	cp -R config/polybar/  ~/.config/polybar/
	@echo "        I3 and Polybar config {OK} "

	#Polyabar Scripts
	cd ~/.config/polybar/modules/ && git clone https://github.com/kamek-pf/polybar-forecast.git
	cd ~/.config/polybar/modules/polybar-forecast/ && cargo build --release
	@echo"				   	Scripts    {OK}"

	#ranger config
	mkdir ~/.config/ranger
	cp config/rc.conf  ~/.config/ranger/
	@echo "     Ranger config file setup {OK} "

	#vim config setup
	mkdir ~/.vim
	cp vim/.vimrc ~/.vimrc
	cp -r vim/.vim/ ~/
	@echo "             Vim setup {OK} "

   #light(brithness control)
	cd ~/Github/prog/ && git clone https://github.com/haikarainen/light
	cd ~/Github/prog/light && ./autogen.sh && ./configure && sudo make

	#setup font
	sudo cp -R config/fonts/source-code-pro /usr/local/share/fonts/
	@echo "           Fount setup {OK} "

	#Urxvt-perls
	mkdir ~/.urxvt/{ext}
	cd config && cp urxvt-resize-font/resize-font, urxvt-perls/{keyboard-select}/deprecated/{clipboard, url-select} ~/.urxvt/ext/
	@echo"                   URXVT perls {OK}"

	#setup Xresources
	cp config/.Xresources ~/.Xresources  && xrdb -laod ~/.Xresources
	@echo "     Xresources setup and loaded {OK} "

	#setup gitignore global file
	cp config/.gitignore_global  ~/
	@echo "       Gitignore global setup {OK} "
	cp config/.gitconfig ~/
	@echo "              Gitconfig setup {OK} "

	#setup background image
	cp config/blue-hair-girl.jpg ~/.config/wallpaper.jpg
	@echo "              Wallppaer setup {OK} "

	#Themes
	cd gtk-themes/gtk && tar -xvf Midnight-BlueNight-Theme.tar.xz
	sudo mv Midnight-BlueNight-Theme.tar.xz /usr/share/themes/
	cd config/ && cp -r gtk-2.0 gtk-3.0 ~/.config
	cp gtkrc-2.0 ~/.gtkrc-2.0
	@echo "   GTK themes setup {OK} "

	#SYSTEMCTL INIT
	sudo systemctl enable NetworkManager.service
	sudo systemctl enable gdm.service

#	#UBUNTU
#
#	#Packages
#	sudo apt update && sudo apt upgrade && sudo apt install git feh vlc vim calcurse audacity htop python-pip cargo gnome-calculator smartmontools nemo nemo-fileroller rxvt-unicode-256color sl bmon clang qdirstat w3m w3m-img docker docker-compose gparted tree ranger xrandr xsel xclip lxappearance gtk-chtheme mailutils libgnutls28-dev gnutls-bin build-essential texinfo libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev libxpm-dev autoconf libclang-dev
#
#
#	#PPAS
#	sudo add-apt-repository ppa:umang/indicator-stickynotes
#	sudo apt-get update
#	sudo apt-get install indicator-stickynotes
#	sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y
#	sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y
#	sudo add-apt-repository ppa:graphic-drivers/ppa
#
#	#PIP
#	pip install --upgrade setuptools
#	pip install wheel
#	sudo apt-get install python3 python3-dev python3-pip libpng-dev libjpeg-dev p7zip-full python3-pip install --user --upgrade pillow python-slugify psutil pyqt5 raven
#	sudo apt install python3-gi python3-setuptools python3-stdeb
#
#   #I3 INSTALL
#	 cd ~/Github/prog/ && sudo apt install git libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev autoconf libxcb-xrm0 libxcb-xrm-dev automake libxcb-shape0-dev -y && git clone https://www.github.com/Airblader/i3 i3-gaps && cd i3-gaps && autoreconf --force --install && rm -rf build/ && mkdir -p build && cd build/ && ../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers && make && sudo make install
#	@echo "   	I3 gaps  install {OK}"
#
#	#POLYBAR INSTALL
#	sudo apt-get install cmake cmake-data libcairo2-dev libxcb1-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-image0-dev libxcb-randr0-dev libxcb-util0-dev libxcb-xkb-dev pkg-config python-xcbgen xcb-proto libxcb-xrm-dev libasound2-dev libmpdclient-dev libiw-dev libcurl4-openssl-dev libpulse-dev libxcb-composite0-dev xcb libxcb-ewmh2 -y && cd ~/Github/prog/ && git clone https://github.com/jaagr/polybar.git && cd ~/Github/prog/polybar && sudo ./build.sh
#	@echo "	    Polybar install {OK}"
#
#	#RANGER INSTALL
#	cd ~/Github/prog/ && git clone https://github.com/hut/ranger.git
#	cd ~/Github/prog/ranger && sudo make install
#	@echo " Ranger {OK}"

	#Hide password when typing (linux mint)
#	sudo mv /etc/sudoers.d/0pwfeedback /etc/sudoers.d/0pwfeedback.disabled
