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

emacs:
	cp -r emacs/.emacs.d  ~/.emacs.d/
	@echo "Emacs config {OK} "

config:
	#vim config setup
	cp vim/.vimrc ~/.vimrc
	cp -r vim/.vim/ ~/
	@echo " Vim setup {OK} "
	#setup font
	cp -R config/fonts/source-code-pro /usr/local/share/fonts/
	@echo " Fount setup {OK} "
	#setup Xresources
	cp config/.Xresources ~/ && xrdb -laod .Xresources
	@echo "Xresources setup and loaded {OK}	"
	#ranger config
	cp config/rc.conf  ~/.config/ranger/
	@echo "Ranger config file setup {OK} "
	#setup gitignore global file
	cp config/.gitignore_global  ~/
	@echo "Gitignore global setup {OK} "
	cp config/.gitconfig ~/
	@echo "Gitconfig setup {OK} "
	#setup i3 and polybar
	cp config/i3/config ~/.config/i3/
	cp -R config/polybar/  ~/.config/polybar/
	@echo " I3 and Polybar config {OK} "
	#setup background image
	cp config/blue-hair-girl.jpg ~/.config/wallpaper.jpg
	@echo "Wallppaer setup {OK} "
	#setup directory
	cd ~/ && mkdir Pictures, Videos, Downloads, Documents, Github, Torrents, Mangas
	@echo "Directory tree {OK}"


themes:
	cd gtk-themes/gtk && tar -xvf Midnight-BlueNight-Theme.tar.xz
	sudo mv Midnight-BlueNight-Theme.tar.xz /usr/share/themes/
	cd config/ && cp gtk-2.0 gtk-3.0 ~/.config
	cp gtkrc-2.0 ~/.gtkrc-2.0





#UBUNTU
#	sudo apt update
#	sudo apt upgrade
#
#	sudo apt install git feh vlc vim audacity htop gnome-calculator nemo nemo-fileroller rxvt-unicode-256color sl bmon qdirstat w3m w3m-img docker docker-compose gparted tree ranger arandr
#
#	sudo add-apt-repository ppa:umang/indicator-stickynotes
#	sudo apt-get update
#	sudo apt-get install indicator-stickynotes
#
#	sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y
#	sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y
#
#	#polybar install
#	#dependencies
#	apt install build-essential git cmake cmake-data pkg-config python3-sphinx libcairo2-dev libxcb1-dev libxcb-util0-dev libxcb-randr0-dev libxcb-composite0-dev python-xcbgen xcb-proto libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev
#	apt install libxcb-xkb-dev libxcb-xrm-dev libxcb-cursor-dev libasound2-dev libpulse-dev libjsoncpp-dev libmpdclient-dev libcurl4-openssl-dev libnl-genl-3-dev
#
#	cd $HOME/ && git clone --recursive https://github.com/polybar/polybar
#	cd polybar
#	mkdir build
#    cd build
#    cmake ..
#    make -j$(nproc)
#    # Optional. This will install the polybar executable in /usr/local/bin
#    sudo make install
#
#	#gtk-config
#	sudo apt install lxappearance gtk-chtheme gt4-gtconfig


install:
	#ARCH
	sudo pacman -S  man vim rxvt-unicode rxvt-unicode-terminfo urxvt-perls xorg-xset nemo nemo-fileroller sl feh  vlc htop gnome-calculator noto-fonts-cjk noto-fonts-emoji noto-fonts  clang i7z cpupower  alsa 
	#graphic drives 
	sudo pacman -S xf86-video-intel vulkan-intel
	#AUR packages
	mkdir ~/AUR && cd ~/AUR
	git clone 	https://aur.archlinux.org/laptop-mode-tools.git
	git clone   https://aur.archlinux.org/batterymon-clone.git	
	git clone   https://aur.archlinux.org/thermald.git 
    git clone   https://aur.archlinux.org/network-ups-tools.git
	git clone   https://aur.archlinux.org/tlpui-git.git
	git clone   https://aur.archlinux.org/polybar.git
	#gtk-config
	sudo pacman -S lxappearance gtk-chtheme xorg-xinpu xorg-xset
	#laptop config
	acpi libinput xf86-input-synaptics xorg-xinput powertop xfce4-power-manager 
	#other config 
	config
	emacs
	themes








