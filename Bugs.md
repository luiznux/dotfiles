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

Sistema
-------

### Shutdown demorado Ubuntu 


**Fonte:** https://medium.com/@sbyang/slow-shut-down-of-ubuntu-18-04-e5fcc31255e2

* No ubuntu 18.04 o sistema demora muito para desligar, conseguimos arrumar isso editando o arquivo *system.conf*

1. Abra o arquivo:

```
sudo vim /etc/systemd/system.conf
```

2. Mude  *DefaultTimeoutStopSec* para um valor entre 4 ou 5 segundos.

```
#DefaultTimeoutStopSec=90s
DefaultTimeoutStopSec=4s
```

3. Reinicie o sistema.



Ranger
------

**Fonte** https://github.com/ranger/ranger/issues/1071

Um bug muito estranho que acontece somente com o gnome e o xfce terminal, basicamente abrindo o ranger com um deles, utilizar o img-preview com o w3m vizualizando varias imagens ou outros arquivos, a imagem fica presa na tela ou é sobreposta por outras imagens.

![bug](https://github.com/luiznux/luiznux-config/blob/master/images/bug-ranger-img-preview.gif)


* A solução é editar o arquivo *actions.py*:

1. Localize o arquivo:

**OBS** *O Diretorio depende da sua versão do python.*
 
> ```sudo *editor* /usr/local/lib/*python2.7*/dist-packages/ranger/core/actions.py```

> Caso não encontre, tente o comando ``` locate actions.py```

2. No arquivo adicione na antes da *linha 491* o codigo:

* Aqui é necessario um teste, pois se adicionar o codigo ``self.redraw_window()``  seu terminal pode ficar piscando apos visualizar uma imagem com o ranger, neste caso adicione ``self.ui.win.redrawwin()`` ao em vez.

 
 ```python

   487         cwd = self.thisdir
   488         kw.setdefault('cycle', self.fm.settings['wrap_scroll'])
   489         kw.setdefault('one_indexed', self.fm.settings['one_indexed'])
   490         direction = Direction(kw)
   491         if 'left' in direction or direction.left() > 0:
   492             steps = direction.left()
   493             if narg is not None:
   494                 steps *= narg
   495             directory = os.path.join(*(['..'] * steps))
   496             self.thistab.enter_dir(directory)
   497             self.change_mode('normal')

```

 Então:

 ```python

   487         cwd = self.thisdir
   488         kw.setdefault('cycle', self.fm.settings['wrap_scroll'])
   489         kw.setdefault('one_indexed', self.fm.settings['one_indexed'])
   490         direction = Direction(kw)
   491         #self.ui.win.redrawwin() usar em caso de bug
   492         self.redraw_window()
   493         if 'left' in direction or direction.left() > 0:
   494             steps = direction.left()
   495             if narg is not None:
   496                 steps *= narg
   497             directory = os.path.join(*(['..'] * steps))
   498             self.thistab.enter_dir(directory)
   499             self.change_mode('normal')
```


3. Salve e reinicie o ranger para aplicar as mudanças.

  



---------------------------------------------------------------------------
verificar --> dmesg
 solução--> https://ubuntuforums.org/showthread.php?t=2400299




