Programas que luiznux possui XUBUNTU 18.04 LTS 64BITS
=====================================================






###Spotify  
----------

* sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90

* echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

* sudo apt-get update

* sudo apt-get install spotify-client



###Team speak
-------------

* sudo add-apt-repository ppa:materieller/teamspeak3

* sudo apt update

* sudo apt install teamspeak3-client



###Discord 
----------

* fazer download do .deb do site oficial

* wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb" 

* cd Dowloads/; sudo dpkg -i discord.deb;



###Virtual Box 
--------------

* baixar o .deb do site oficial 

* https://www.virtualbox.org/wiki/Linux_Downloads



Video e Imagem
--------------


### Gimp 


qualquer lugar kkkk


### VLC 

* sudo apt install vlc


### Kdenlive 

* sudo add-apt-repository ppa:kdenlive/kdenlive-stable

* sudo apt-get install kdenlive

--------
### Htop 
--------

* sudo apt install htop


### WPS-Office 
--------------

* versao de 2016(acho mais bonita e simples)

* wget -O wps_office_2016.deb ""http://kdl.cc.ksosoft.com/wps-community/download/6758/wps-office_10.1.0.6758_amd64.deb

* cd Downloads/ && sudo dpkg -i wps_office_2016.deb

* instalar pacotes de linguagem para PT-BR(procurar na net)



Editores de Texto
----------------


### Vim 

* sudo apt install vim


## Emacs 

* sudo apt install emacs


### Atom 

* sudo apt install atom


--------

Wine 
--------
* sudo apt install wine-stable


### Play on Linux 

* sudo apt install playonlinux


### light 
---------
--> configurar brilho

* ler o README e seguir as intruções para o arquivo tar.gz em relaeases
    
    >https://github.com/haikarainen/light

    >https://github.com/haikarainen/light/releases

    > tar xf light-x.yy.tar.gz
      cd light-x.yy/
      ./configure && make                                      
      sudo make install




### Boot-Repair 
---------------
--> faz milagres com seu boot quebrado

* sudo add-apt-repository ppa:graphics-drivers

* sudo add-apt-repository ppa:yannubuntu/boot-repair

* sudo apt-get install boot-repair


### Sticky notes 
----------------
* sudo add-apt-repository ppa:umang/indicator-stickynotes

* sudo apt-get update

* sudo apt-get install indicator-stickynotes


### Grub Customizer  -->Customizando seu grub desde não sei quando...
---------------------------------------------------------------------
* CUIDADO USANDO ISSO, PODE QUEBRAR SEU GRUB !!!

* sudo add-apt-repository ppa:danielrichter2007/grub-customizer && sudo apt-get update && sudo apt-get install grub-customizer


### Docker and Docker Compose 
-----------------------------
* sudo apt install docker

* sudo apt install docker-compose


### TLP and TPLUI(interface grafica pro tlp) 
--------------------------------------------
* sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y

* sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y


### Multibootusb 
----------------
* https://github.com/mbusb/multibootusb/releases


### Xfce-goodies 
----------------
* sudo apt-get install xfce-goodies 


### Python 
----------
* INSTALAR PIP3

    > sudo apt update && sudo apt install python3-pip && pip3 --version


* Spyder << uma boa IDE para python

    > sudo apt-get install spyder3


--> para todos os meus projetos utilizo virtualenvs para evitar conflitos de libs 


* sudo pip3 install virtualenv

* libs utilizadas: USAR APENAS NAS ENVS!!! (a nao ser que você queira quebrar o pc)
    
   * sudo pip3 install jupyter
   * sudo pip3 install matplotlib
   * sudo pip3 install scipy
   * sudo pip3 install pandas


### Navegadores 
---------------

* Opera
    
    * po mano procura ai na net o .deb vai ....

* Chrome e Chromium
    
    * mesma coisa q o de cima 


* w3m (terminal browser)
    
    * sudo apt-get install w3m 


* browsh (terminal browser)
    
    * wget -o browsh.deb "https://github.com/browsh-org/browsh/releases/download/v1.5.2/browsh_1.5.2_linux_amd64.deb"

    * sudo dpkg -i browsh.deb 


* Elinks (termial browser)
    
    * sudo apt install elinks






