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

install:
	sudo apt update
	sudo apt upgrade

	sudo apt install git feh vlc vim audacity htop gnome-calculator nemo nemo-fileroller rxvt-unicode-256color sl bmon qdirstat w3m w3m-img docker docker-compose gparted

	sudo add-apt-repository ppa:umang/indicator-stickynotes
	sudo apt-get update
	sudo apt-get install indicator-stickynotes

	sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y
	sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y

	config
	emacs
