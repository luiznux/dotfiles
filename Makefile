emacs:
	
	cp -r emacs/.emacs.d  ~/.emacs.d/

config:
	cp vim/.vimrc ~/.vimrc
	cp -r fonts/source-code-pro /usr/local/share/fonts


install:
	sudo apt update
	sudo apt upgrade
	sudo apt install vlc vim audacity htop gnome-calculator nemo nemo-fileroller rxvt-unicode-256color sl bmon qdirstat w3m w3m-img docker docker-compose gparted
	sudo add-apt-repository ppa:umang/indicator-stickynotes
	sudo apt-get update
	sudo apt-get install indicator-stickynotes
	sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y
	sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y
	emacs
