BUGS
====

* Este arquivo possui uma série de correções de bugs que encontrei com o uso de minha distro com as configurações que possuo.

* Estão organizadas por tituto



- [VIM](#vim)

- [WPS-OFFICE](#wps-office)

- [BLUETOOTH](#bluetooth)

- [MOUSE](#mouse)


---
VIM
---


###  Vim abrindo com o terminal padrão

**Fonte:**  https://askubuntu.com/questions/788736/open-vim-in-xfce4-terminal-from-thunarA

**Solução:**

1. editar o arquivo  /usr/share/applications/vim.desktop 
 
2. Mudar o valor de *'Exec'* adicionando

    ```
    Exec=xfce4-terminal -e "vim %F" 
   
    ```
 3. Então mudar o valor de *'Terminal'*  para  *false*.

    ``` 
    Terminal=false 
    ```

-------------------------------------------------------------------------------------------


WPS-OFFICE
----------

### Fundo preto no WPS

**Fonte:** https://github.com/horst3180/arc-theme/issues/748

**Solução:**

* Para corrigir nos 3 programas(Writer, Spreadsheets e Presentatio), basta alterar os 3 binarios :

```
sudo vim /usr/bin/et (Spreadsheets)

sudo vim /usr/bin/wps(Writer)

sudo vim /usr/bin/wpp (Presentatio)
```

1. Procure:

    ```
    ${gInstallPath}/office6/${gApp} ${gOptExt} ${gOpt} "$@"
    ```
2. Substitua por:

    ```
    ${gInstallPath}/office6/${gApp} -style motif ${gOptExt} ${gOpt} "$@"
    ```

3. Reinicie o programa.

---------------------------------------------------------------------------------------------

BLUETOOTH
---------

### Bluetooth iniciando automaticamente

* Vale lembrar que não é uma solução para o problema, apenas desabilito ele quando o sistema inicia, caso queira usar tera que habilitar novamente.

https://ubuntuforums.org/showthread.php?t=2398668

----------------------------------------------------------------------------------------------

Mouse
-----

### Synacptics conflito com drive da dell

Problema consiste em um conflito entre o driver da dell e o synacptics, para corrigilo é necessario desabilitar um deles.

**Fonte:** https://askubuntu.com/questions/773595/how-can-i-disable-touchpad-while-typing-on-ubuntu-16-04-syndaemon-isnt-working

**Solução:**

1. Verificar quais dos drivers estão sendo utilizados 

    ```
    xinput list
    ```

    ```
    ~  ➜ xinput list
⎡ Virtual core pointer                      	id=2	[master pointer  (3)]
⎜   ↳ Virtual core XTEST pointer              	id=4	[slave  pointer  (2)]
⎜   ↳ ELAN Touchscreen                        	id=10	[slave  pointer  (2)]
⎜   ↳ DLL0704:01 06CB:76AE Touchpad           	id=12	[slave  pointer  (2)]
⎜   ↳ SynPS/2 Synaptics TouchPad              	id=14	[slave  pointer  (2)]
⎣ Virtual core keyboard                     	id=3	[master keyboard (2)]
    ↳ Virtual core XTEST keyboard             	id=5	[slave  keyboard (3)]
    ↳ Power Button                            	id=6	[slave  keyboard (3)]
    ↳ Video Bus                               	id=7	[slave  keyboard (3)]
    ↳ Power Button                            	id=8	[slave  keyboard (3)]
    ↳ Sleep Button                            	id=9	[slave  keyboard (3)]
    ↳ Integrated_Webcam_HD                    	id=11	[slave  keyboard (3)]
    ↳ AT Translated Set 2 keyboard            	id=13	[slave  keyboard (3)]
    ↳ Dell WMI hotkeys                        	id=15	[slave  keyboard (3)]
    ```

2. No exemplo acima temos 2 drivers *"↳ SynPS/2 Synaptics TouchPad"* e  *"DLL0704:01 06CB:76AE Touchpad"*,vamos desativar um deles para evitar o conflito.

Abra arquivo **/usr/share/X11/xorg.conf.d/51-synaptics-quirks.conf** como root

3. Insira no final do arquivo:

    ```
    # Disable generic Synaptics device, as we're using
    # "DLL0704:01 06CB:76AE Touchpad"
    # Having multiple touchpad devices running confuses syndaemon
    Section "InputClass"
    Identifier "SynPS/2 Synaptics TouchPad"
        MatchProduct "SynPS/2 Synaptics TouchPad"
        MatchIsTouchpad "on"
        MatchOS "Linux"
        MatchDevicePath "/dev/input/event*"
        Option "Ignore" "on"
    EndSection 
    ```

4. Rebootar o sistema

-----------------------------------------------------------------------------








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


gsettings set org.cinnamon.desktop.default-applications.terminal exec xfce4-terminal
