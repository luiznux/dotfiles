#UBUNTU

#Packages
sudo apt update && sudo apt upgrade && sudo apt install git feh vlc vim calcurse audacity htop python-pip cargo gnome-calculator smartmontools nemo nemo-fileroller rxvt-unicode-256color sl bmon clang qdirstat w3m w3m-img docker docker-compose gparted tree ranger xrandr xsel xclip lxappearance gtk-chtheme mailutils libgnutls28-dev gnutls-bin build-essential texinfo libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev libxpm-dev autoconf libclang-dev


#PPAS
sudo add-apt-repository ppa:umang/indicator-stickynotes
sudo apt-get update
sudo apt-get install indicator-stickynotes
sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y
sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y
sudo add-apt-repository ppa:graphic-drivers/ppa

#PIP
pip install --upgrade setuptools
pip install wheel
sudo apt-get install python3 python3-dev python3-pip libpng-dev libjpeg-dev p7zip-full python3-pip install --user --upgrade pillow python-slugify psutil pyqt5 raven
sudo apt install python3-gi python3-setuptools python3-stdeb

I3 INSTALL
 cd ~/Github/prog/ && sudo apt install git libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev autoconf libxcb-xrm0 libxcb-xrm-dev automake libxcb-shape0-dev -y && git clone https://www.github.com/Airblader/i3 i3-gaps && cd i3-gaps && autoreconf --force --install && rm -rf build/ && mkdir -p build && cd build/ && ../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers && make && sudo make install
@echo "   	I3 gaps  install {OK}"

#POLYBAR INSTALL
sudo apt-get install cmake cmake-data libcairo2-dev libxcb1-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-image0-dev libxcb-randr0-dev libxcb-util0-dev libxcb-xkb-dev pkg-config python-xcbgen xcb-proto libxcb-xrm-dev libasound2-dev libmpdclient-dev libiw-dev libcurl4-openssl-dev libpulse-dev libxcb-composite0-dev xcb libxcb-ewmh2 -y && cd ~/Github/prog/ && git clone https://github.com/jaagr/polybar.git && cd ~/Github/prog/polybar && sudo ./build.sh
@echo "	    Polybar install {OK}"

#RANGER INSTALL
cd ~/Github/prog/ && git clone https://github.com/hut/ranger.git
cd ~/Github/prog/ranger && sudo make install
@echo " Ranger {OK}"

Hide password when typing (linux mint)
sudo mv /etc/sudoers.d/0pwfeedback /etc/sudoers.d/0pwfeedback.disabled



Urxvt-perls
mkdir ~/.urxvt/{ext}
cd config && cp urxvt-resize-font/resize-font, urxvt-perls/{keyboard-select}/deprecated/{clipboard, url-select} ~/.urxvt/ext/
@echo"                   URXVT perls {OK}"

