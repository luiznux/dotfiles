# Dotfiles

![Linux](https://img.shields.io/badge/Linux-FCC624?style=flat&logo=linux&logoColor=black)
![Arch](https://img.shields.io/badge/Arch%20Linux-1793D1?logo=arch-linux&logoColor=fff&style=flat)

![https://i3wm.org/](img/shields/i3-logo-30.png)

This project contains all my desktop config files and more, feel free to use
anything the could help you on your configuration.

The install script only works only in **arch linux**, but the other files may
work fine any GNU Linux distros

## Usage
To install my dotfiles just clone this repository and execute the script.
```
git clone --recurse-submodules -j8 https://github.com/luiznux/dotfiles
```
 This script is intended to run as normal user, but some process need
 sudo privileges to work, so keep an eye out when prompted for your
 password.
```
chmod +x install && ./install
```

## Debugging and catching errors
After the end of the execute the script, a install.log file will be
created as a log file to see eventual errors.


## Screenshot
![luiznux-img](img/screenshot.png)
