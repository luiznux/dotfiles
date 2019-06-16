
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



* pegar as config do emacs do edison neto god, e arrumar as bibliotecas que faltam, arrumar font no init.el heith 139

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
