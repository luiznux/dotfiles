Programas que luiznux possui XUBUNTU 18.04 LTS 64BITS

___________________________________________________________________________________________________________________

### Spotify ### --> baixar do repositorio ofcial spotify (NAO USAR SNAP INSTALL)

>> sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90

>> echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

>> sudo apt-get update

>> sudo apt-get install spotify-client

____________________________________________________________________________________________________________________

### Team speak ###

>> sudo add-apt-repository ppa:materieller/teamspeak3

>> sudo apt update

>> sudo apt install teamspeak3-client

_____________________________________________________________________________________________________________________

### Discord ###

>> fazer download do .deb do site oficial

>> wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb" 

>> cd Dowloads/; sudo dpkg -i discord.deb;

_____________________________________________________________________________________________________________________

### Virtual Box ###

>> baixar o .deb do site oficial 

>> https://www.virtualbox.org/wiki/Linux_Downloads

_____________________________________________________________________________________________________________________

### Gimp ###

qualquer lugar kkkk

_____________________________________________________________________________________________________________________

### VLC ###

>> sudo apt install vlc

____________________________________________________________________________________________________________________

### Kdenlive ###

>> sudo add-apt-repository ppa:kdenlive/kdenlive-stable

>> sudo apt-get install kdenlive

____________________________________________________________________________________________________________________

### Htop ###

>> sudo apt install htop

___________________________________________________________________________________________________________________

### WPS-Office ###

>> versao de 2016(acho mais bonita e simples)

>> wget -O wps_office_2016.deb ""http://kdl.cc.ksosoft.com/wps-community/download/6758/wps-office_10.1.0.6758_amd64.deb

>> cd Downloads/ && sudo dpkg -i wps_office_2016.deb

>> instalar pacotes de linguagem para PT-BR(procurar na net)

___________________________________________________________________________________________________________________

### Vim ###

>> sudo apt install vim

___________________________________________________________________________________________________________________

## Emacs ###

>> sudo apt install emacs

__________________________________________________________________________________________________________________

### Atom ###

>> sudo apt install atom

__________________________________________________________________________________________________________________

### Wine ###

>> sudo apt install wine-stable

___________________________________________________________________________________________________________________

### Play on Linux ###

>> sudo apt install playonlinux

___________________________________________________________________________________________________________________

### light ### --> configurar brilho

>> ler o README e seguir as intruções para o arquivo tar.gz em relaeases
    
    >https://github.com/haikarainen/light

    >https://github.com/haikarainen/light/releases

    > tar xf light-x.yy.tar.gz
      cd light-x.yy/
      ./configure && make                                      
      sudo make install




### Boot-Repair ### --> faz milagres com seu boot quebrado

>> sudo add-apt-repository ppa:graphics-drivers

>> sudo add-apt-repository ppa:yannubuntu/boot-repair

>> sudo apt-get install boot-repair

___________________________________________________________________________________________________________________

### Sticky notes ###

>> sudo add-apt-repository ppa:umang/indicator-stickynotes

>> sudo apt-get update

>> sudo apt-get install indicator-stickynotes

___________________________________________________________________________________________________________________

### Grub Customizer ### -->Customizando seu grub desde não sei quando...

>> CUIDADO USANDO ISSO, PODE QUEBRAR SEU GRUB !!!

>> sudo add-apt-repository ppa:danielrichter2007/grub-customizer && sudo apt-get update && sudo apt-get install grub-customizer

__________________________________________________________________________________________________________________

### Docker and Docker Compose ###

>> sudo apt install docker

>> sudo apt install docker-compose

____________________________________________________________________________________________________________________

### TLP and TPLUI(interface grafica pro tlp) ###

>> sudo add-apt-repository ppa:linrunner/tlp -y && sudo apt update && sudo apt install tlp tlp-rdw -y

>> sudo add-apt-repository ppa:linuxuprising/apps -y && sudo apt update && sudo apt install tlpui -y

____________________________________________________________________________________________________________________

### Multibootusb ###

>> https://github.com/mbusb/multibootusb/releases

____________________________________________________________________________________________________________________

### Xfce-goodies ###

>> sudo apt-get install xfce-goodies 

____________________________________________________________________________________________________________________

### Python ###

>> INSTALAR PIP3  <<

    > sudo apt update && sudo apt install python3-pip && pip3 --version

>> Spyder << uma boa IDE para python

    > sudo apt-get install spyder3

>> para todos os meus projetos utilizo virtualenvs para evitar conflitos de libs <<

>> sudo pip3 install virtualenv

>> libs utilizadas: USAR APENAS NAS ENVS!!! (a nao ser que você queira quebrar o pc)
    
   > sudo pip3 install jupyter
   > sudo pip3 install matplotlib
   > sudo pip3 install scipy
   > sudo pip3 install pandas

___________________________________________________________________________________________________________________

### Navegadores ###

>> Opera
    
    > po mano procura ai na net o .deb vai ....


>> Chrome e Chromium
    
    > mesma coisa q o de cima 
>> w3m (terminal browser)
    
    > sudo apt-get install w3m 

>> browsh (terminal browser)
    
    > wget -o browsh.deb "https://github.com/browsh-org/browsh/releases/download/v1.5.2/browsh_1.5.2_linux_amd64.deb"
    > sudo dpkg -i browsh.deb 

>> Elinks (termial browser)
    
    > sudo apt install elinks

    ____________________________________________________________________________________________________________________





**BUG vim abrindo com o terminal padrao
https://askubuntu.com/questions/788736/open-vim-in-xfce4-terminal-from-thunarA

I edited /usr/share/applications/vim.desktop and changed the value for 'Exec' adding

Exec=xfce4-terminal -e "vim %F"
just as jbrock said. Then I changed the value for 'Terminal' to false.

Terminal=false
It worked for me.
********************
______________________________________________________



gsettings set org.cinnamon.desktop.default-applications.terminal exec xfce4-terminal
________________________________________________________________
WpsOffice( procura zip com pacotes de linguagem)
--> BUG DO FUNDO PRETO
https://github.com/horst3180/arc-theme/issues/748
Here is a solution that works across reboots, but probably not across subsequent installs of WPS:
sudo vim /usr/bin/et (excel)
sudo vim /usr/bin/wps(word)
sudo vim /usr/bin/wpp (powepint)

Find:
${gInstallPath}/office6/${gApp} ${gOptExt} ${gOpt} "$@"
Replace with:
${gInstallPath}/office6/${gApp} -style motif ${gOptExt} ${gOpt} "$@"




_______________________________________________________________________

--> Desabilitar o bluetooth( on start up)
https://ubuntuforums.org/showthread.php?t=2398668

________________________________________________________________________________________________________________
---> configurar synacptics bug com mouse pad da dell(dois drivers dao conflito)
https://askubuntu.com/questions/773595/how-can-i-disable-touchpad-while-typing-on-ubuntu-16-04-syndaemon-isnt-working



>> pegar as config do emacs do edison neto god, e arrumar as bibliotecas que faltam, arrumar font no init.el heith 139

_______________________________________________________________________________
verificar --> dmesg
 solução--> https://ubuntuforums.org/showthread.php?t=2400299

________________________________________________________________________________
>>How to fix slow shutdown / restart of Ubuntu 18.04

Sometimes, Ubuntu 18.04 system hangs or takes very long time to shutdown.

We can fix this by editing the system.conf file.

For example,

$ sudo vim /etc/systemd/system.conf

Change the DefaultTimeoutStopSec section to around 4 or 5 seconds.

#DefaultTimeoutStopSec=90s
DefaultTimeoutStopSec=4s
Save the file, and reboot. :)

link --> https://medium.com/@sbyang/slow-shut-down-of-ubuntu-18-04-e5fcc31255e2
________________________________________________________________________________
