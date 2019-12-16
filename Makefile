emacs:
	
	cp -r emacs/.emacs.d  ~/.emacs.d/


system:
	cp vim/.vimrc ~/.vimrc
	cp -r vim/.vim/ ~/
	cp -r system/fonts/source-code-pro /usr/local/share/fonts/
	cp system/.Xresources ~/
	cp system/.gitignore_global  ~/



install:
	sudo apt update
	sudo apt upgrade
	sudo apt install git vlc vim audacity htop gnome-calculator nemo nemo-fileroller rxvt-unicode-256color sl bmon qdirstat w3m w3m-img docker docker-compose gparted
	sudo add-apt-repository ppa:umang/indicator-stickynotes
	sudo apt-get update
	sudo apt-get install indicator-stickynotes
	sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y
	sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y
	system
	emacs
