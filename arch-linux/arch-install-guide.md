<!--toc:start-->
- [Arch linux Install guide](#arch-linux-install-guide)
  - [My config](#my-config)
    - [GRUB config](#grub-config)
    - [Systemd-boot](#systemd-boot)
  - [1. Basics](#1-basics)
  - [2. Load crypt modules](#2-load-crypt-modules)
  - [3. Create particions](#3-create-particions)
    - [UEFI](#uefi)
      - [GRUB](#grub-partition)
      - [Systemd-boot](#systemd-boot-partition)
    - [Non-UEFI](#non-uefi)
  - [4. Encrypt the "LINUX LVM" particion](#4-encrypt-the-linux-lvm-particion)
  - [5. Open your crypt](#5-open-your-crypt)
  - [6. Create a Physical Volume (PV)](#6-create-a-physical-volume-pv)
  - [7. Create Volume Group (VG)](#7-create-volume-group-vg)
  - [8. Create a Logical Volume (LV)](#8-create-a-logical-volume-lv)
  - [9. Create the other LV for "/"](#9-create-the-other-lv-for)
  - [10. Format particions](#10-format-particions)
  - [11. Mount particions](#11-mount-particions)
  - [12. Packages](#12-packages)
  - [13. Mv chroot to /mnt](#13-mv-chroot-to-mnt)
  - [14. Install some packages](#14-install-some-packages)
  - [15. Set locale](#15-set-locale)
  - [16. Hosts and users](#16-hosts-and-users)
  - [18. BootLoader](#18-bootloader)
    - [Grub](#grub-bootloader)
    - [Systemd-boot + UKI](#systemd-boot-bootloader)
  - [19. Exit and restart](#19-exit-and-restart)
  - [20. Post install](#20-post-install)
    - [Secure Boot](#secure-boot)
    - [Adding other OS(windows) to boot options](#adding-other-oswindows-to-boot-options)
    - [Set CPU frequency scaling](#set-cpu-frequency-scaling)
<!--toc:end-->


-----------------

# Arch linux Install guide
```ascii

 █████╗ ██████╗  ██████╗██╗  ██╗
██╔══██╗██╔══██╗██╔════╝██║  ██║
███████║██████╔╝██║     ███████║
██╔══██║██╔══██╗██║     ██╔══██║
██║  ██║██║  ██║╚██████╗██║  ██║
╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝
```
This archive contains some of instructions for install arch linux distro.
Remembering that all the steps that I will take can be changed at your discretion
without any problem. The great advantage of this distro is that each one adapts the
process to their needs, so feel free to change anything.
The following steps serve more as an example of how I do my installation, so your best
friend will be the [ArchWiki](https://wiki.archlinux.org/), where 99% of your problems
and doubts will be there.

## My config
My configuration is not very sophisticated, I just partition with a separate home
directory and two other boot partitions. In addition, I use LUKS encryption, together
with a  LVM (Logical Volume Management), so the data is organized and secure.
The figure below illustrates how my partitioning is configured.

Note: I recently switched from GRUB to systemd-boot and to keep GRUB as a fallback, only systemd-boot options were added.


### GRUB config
```
+----------------+ +----------------+ +-------------------------------------------------------------------+
| Boot partition | | Boot partition | |       Logical volume 1          |        Logical volume 2         |
|                | |                | |                                 |                                 |
| /boot/efi      | | /boot          | |            /                    |     [SWAP FOR HIBERNATE]        |
|                | |                | |                                 |                                 |
|                | |                | |     /dev/MyVolGroup/root        |     /dev/MyVolGroup/swap        |
|                | |                | |_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _|
|                | |                | |                                                                   |
|                | |                | |                     LUKS1 encrypted partition                     |
|  /dev/sda1     | |  /dev/sda2     | |                           /dev/sda3                               |
+----------------+ +----------------+ +-------------------------------------------------------------------+
|                                              /dev/sda                                                   |
-----------------------------------------------------------------------------------------------------------
```


### Systemd-boot

```
+---------------------------------+ +-------------------------------------------------------------------+
|         Boot partition          | |       Logical volume 1          |        Logical volume 2         |
|                                 | |                                 |                                 |
|            /boot                | |            /                    |     [SWAP FOR HIBERNATE]        |
|                                 | |                                 |                                 |
|          /dev/sda1              | |     /dev/MyVolGroup/root        |     /dev/MyVolGroup/swap        |
|                                 | |_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _|_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _|
|                                 | |                                                                   |
|                                 | |                     LUKS1 encrypted partition                     |
|                                 | |                           /dev/sda2                               |
+---------------------------------+ +-------------------------------------------------------------------+
|                                              /dev/sda                                                 |
---------------------------------------------------------------------------------------------------------
```

**Font**  https://wiki.archlinux.org/index.php/Dm-crypt/Encrypting_an_entire_system#LUKS_on_LVM

So lets get started !


## 1. Basics

* set keyboard layout
``` bash
$ loadkeys br-abnt2
```

* edit mirror list
```bash
vim /etc/pacman.d/mirrorlist
```

* update
```bash
$ pacman -Sy
```

## 2. Load crypt modules

```bash
$ modprobe -a dm-mod dm-crypt
```

## 3. Create particions


### UEFI

<a id="grub-partition"></a>
#### GRUB
* In case of using uefi bios, use GPT partition table with a separate boot partition

```bash
$ fdisk -l && cfdisk /dev/sdx
```
|Device       |Type           | Size     | Format   |
| ----------- |:-------------:|:--------:|:--------:|
| `/dev/sda1` | BIOS BOOT(EFI)| 500MB    |FAT32     |
| `/dev/sda2` | BOOT          | 500MB    |EXT4      |
| `/dev/sda3` | LINUX LVM     |  any     |EXT4      |

<a id="systemd-boot-partition"></a>
#### Systemd-boot

```bash
$ fdisk -l && cfdisk /dev/sdx
```
|Device       |Type           | Size     | Format   |
| ----------- |:-------------:|:--------:|:--------:|
| `/dev/sda1` | EFI SYSTEM    | 700MB    |FAT32     |
| `/dev/sda2` | LINUX LVM     |  any     |EXT4      |

<a id="non-uefi-partition"></a>
### Non-UEFI
* In case of using a non-uefi bios, use MRB(or DOS) partition table with a separe boot particion too

```bash
$ fdisk -l && cfdisk /dev/sdx
```

**NOTE**: You will need to specify the boot partition when using cfdisk in MRB(or DOS) table, in this case is /dev/sda1

|Device       |Type           | Size     | Format   |
| ----------- |:-------------:|:--------:|:--------:|
| `/dev/sda1` | BOOT          | 500MB    |EXT4      |
| `/dev/sda2` | LINUX LVM     |  any     |EXT4      |

**OBS**: if you misstype the partition table type with cfdisk, use **wipefs** to clear old tables
```bash
$ wipefs -a /dev/sdX
```


## 4. Encrypt the "LINUX LVM" particion

```bash
$ cryptsetup -y -v luksFormat --type luks1 -c aes-xts-plain64 -s 512 /dev/sda3
```

* just remember that a password will be required


## 5. Open your crypt

```bash
$ cryptsetup open  --type luks /dev/sda3 linux
```

## 6. Create a Physical Volume (PV)

* create one

```bash
$ pvcreate /dev/mapper/linux
```
* to see info about it

```bash
$ pvs
```

## 7. Create Volume Group (VG)


```bash
$ vgcreate linux /dev/mapper/linux
```


## 8. Create a Logical Volume (LV)
**NOTE**: swap must be bigger than your ram size
```bash
$ lvcreate -L 8G linux -n swap
```

* to see lv

```bash
$ lvs
```

## 9. Create the other LV for "/"

```bash
$ lvcreate -l +100%FREE linux -n archlinux
```

* activate the volumes

```bash
$ vgchange -ay
```

## 10. Format particions


* BOOT, crypt

**UEFI**
```bash
$ mkfs.fat -F32 /dev/sda1
$ mkfs.ext4 /dev/sda2
```

**NON-UEFI**
```bash
$ mkfs.ext4 /dev/sda1
```

**Crypt**
```bash
$ mkfs -t ext4 /dev/mapper/linux-archlinux

$ mkswap /dev/mapper/linux-swap
$ swapon /dev/mapper/linux-swap
$ lsblk -f
```

## 11. Mount particions

**UEFI-GRUB**
```bash
$ mount /dev/mapper/linux-archlinux /mnt

$ mkdir /mnt/boot/
$ mount /dev/sda2 /mnt/boot

$ mkdir /mnt/boot/efi
$ mount /dev/sda1 /mnt/boot/efi
```

**UEFI-Systemd-boot**
```bash
$ mount /dev/mapper/linux-archlinux /mnt

$ mkdir /mnt/boot/
$ mount /dev/sda2 /mnt/boot/
```

**NON-UEFI**
```bash
$ mount /dev/mapper/linux-archlinux /mnt

$ mkdir /mnt/boot/
$ mount /dev/sda1 /mnt/boot
```

## 12. Packages

* edit mirror list

```bash
$ vim /etc/pacman.d/mirrorlist
```

* Install the base packages and genfstab

```bash
$ pacstrap -i /mnt base-devel base linux linux-firmware linux-firmware-qlogic

$ genfstab -U -p /mnt >> /mnt/etc/fstab
```


## 13. Mv chroot to /mnt


```bash
$ arch-chroot /mnt /bin/bash
```


## 14. Install some packages

```bash
$ pacman -S bash-completion gvim sudo os-prober wireless_tools networkmanager network-manager-applet mtools wpa_supplicant dosfstools dialog lvm2 linux-headers ntfs-3g --noconfirm
```

## 15. Set locale

```bash
$ rm -f /etc/localtime
$ ln -s /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime

$ vim /etc/locale.gen
$ locale-gen
$ echo KEYMAP=br-abnt2 >> /etc/vconsole.conf
```


## 16. Hosts and users

```bash
$ echo "arch" > /etc/hostname
```

```bash
$ echo "127.0.0.1      localhost.localdomain            localhost
::1            localhost.localdomain            localhost
127.0.1.1      arch.localdomain                 arch" \
> /etc/hosts
```

```
$ passwd
```

* Add a user

```bash
$ useradd -m -g users -G wheel,games,power,optical,storage,scanner,lp,audio,video -s /bin/bash luiznux

$ passwd luiznux

$ echo "luiznux ALL=(ALL)ALL" >> /etc/sudoers
```

## 18. BootLoader

<a id="grub-bootloader"></a>
### Grub

1. Install packages

**UEFI**
```bash
$ pacman -S grub efibootmgr --noconfirm
```
- Edit mkinitcpio
```bash
$ vim /etc/mkinitcpio.conf
```

* look for HOOKS="...." and add:

```bash
HOOKS=(base udev keyboard autodetect keymap consolefont modconf block lvm2 encrypt resume filesystems fsck)
```
* **ADD THIS AT THE SAME ORDER**

* save, exit and reload

```bash
$ mkinitcpio -p linux
```

**NON-UEFI**
```bash
$ pacman -S grub --noconfirm
```

2. CONFIG

```bash
$ vim /etc/default/grub
```

* look for  GRUB_CMDLINE_LINUX="", and the set:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=/dev/sda3:linux:allow-discards resume=/dev/mapper/linux-swap quiet splash"
```

For notebooks that you want to disable the graphic card, add
```bash
pci=nomsi
```

__OBS__ Use this flag to enable full cpu temperature for lm\_sensors
```bash
GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=/dev/sda3:linux:allow-discards resume=/dev/mapper/linux-swap quiet splash acpi_enforce_resources=lax"
```

3. after that, write or update those variables to:
```bash
GRUB_PRELOAD_MODULES="lvm..."

GRUB_ENABLE_CRYPTODISK=y

GRUB_DISABLE_SUBMENU=y
```

__OBS__ If you are using dualboot config with windows and installed "os-prober" already package, you will need to add the following line for grub identify the other S.O:
```bash
#Enable os-prober
GRUB_DISABLE_OS_PROBER=false
```

4. Installing

**UEFI**
```bash
$ grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub --recheck
$ mkdir -p /boot/grub/locale/
$ cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
```

**NON UEFI**
```bash
$ grub-install --target=i386-pc /dev/sda1 --recheck
$ mkdir -p /boot/grub/locale/
$ cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
```

5. Gen grub file

```bash
$ grub-mkconfig -o /boot/grub/grub.cfg
```

<a id="systemd-boot-bootloader"></a>
### Systemd-boot + UKI

1. Edit mkinitcpio

```bash
$ vim /etc/mkinitcpio.conf
```

* **ADD THIS AT THE SAME ORDER**
```bash
HOOKS=(base systemd autodetect microcode modconf keyboard sd-vconsole block lvm2 sd-encrypt filesystems fsck)
```

2. Enable UKI layout

* Create the file `/etc/kernel/install.conf` with the following content:
```bash
$ sudo vim /etc/kernel/install.conf
```
```
layout=uki
uki_generator=mkinitcpio
```

* Edit the file `/etc/mkinitcpio.d/linux.preset`
```bash
$ sudo vim /etc/mkinitcpio.d/linux.preset
```
```
# mkinitcpio preset file for the 'linux' package

#ALL_config="/etc/mkinitcpio.conf"
ALL_kver="/boot/vmlinuz-linux"
#ALL_kerneldest="/boot/vmlinuz-linux"

#PRESETS=('default')
PRESETS=('default' 'fallback')

#default_config="/etc/mkinitcpio.conf"
default_image="/boot/initramfs-linux.img"
default_uki="/boot/EFI/Linux/arch-linux.efi"
#default_options="--splash /usr/share/systemd/bootctl/splash-arch.bmp"

fallback_config="/etc/mkinitcpio.conf"
fallback_image="/boot/initramfs-linux-fallback.img"
fallback_uki="/boot/EFI/Linux/arch-linux-fallback.efi"
fallback_options="-S autodetect"
```

3. Create kernel command line

* Get the current id from the crypt device and swap partition
```bash
$ sudo lsblk -f /dev/sda
```

* Create the file  `/etc/kernel/cmdline` with the current content:

**One line only (line breaks here are just for readability)**
```
rd.luks.name=CRYPT_DEVICE_ID=cryptroot \
rd.luks.options=discard,password-echo=no,tries=15,timeout=0 \
root=/dev/mapper/linux-archlinux \
resume=UUID=SWAP_ID \
rw quiet loglevel=3
```

4. Generate kernel image
```bash
$ sudo sudo mkinitcpio -P
```

3. Fix ESP permissions if necessary

```bash
$ sudo chmod 700 /boot
$ sudo chown root:root /boot
```

4. Install and check status

```bash
$ sudo bootctl install
$ sudo bootctl status
```

5. Configure loader defaults
```bash
$ sudo vim /boot/efi/loader/loader.conf
```
```
timeout 180
editor  yes
auto-entries yes
```

8. Enable auto-update
With systemd or pacman
```bash
$ sudo systemctl enable systemd-boot-update.service
```

9. For secure boot, see [post install](#20-post-install)

## 19. Exit and restart

* Remember to enable network
```bash
$ systemctl enable NetworkManager
```
* Exit
```bash
$ exit
$ umount -R /mnt
$ systemctl reboot
```

## 20. Post install

### Secure Boot

1. Clean the secure boot keys on your MotherBoard BIOS

 * Enter in the BIOS and select the option to "clean know keys"

 * Then, reboot

2. Install sbctl
```bash
$ sudo pacman -Syu sbctl
```

3. Enroll your keys with windows
```bash
$ sudo sbctl enroll-keys -m
```

4. Reboot the system and verify
```bash
$ sudo sbctl verify
```

5. Enable auto-signing
```bash
$ sudo sbctl sign -s -o /usr/lib/systemd/boot/efi/systemd-bootx64.efi.signed /usr/lib/systemd/boot/efi/systemd-bootx64.efi
```

6. Reboot and enable secureboot from the Mother board

### Adding other OS(windows) to boot options

1. Find the device/partition that windows UEFI are located and has the `bootmgfw.efi` file, usually is FAT partition
 ```bash
 $ lsblk -f
 ```
* ex: /dev/sda1

2. Mount the partition into `/mnt`
 ```bash
 $ sudo mount /dev/sda1 /mnt
 ```

3. Copy the Microsoft folter to boot entries
*  ex: /mnt/EFI/Microsoft/
 ```bash
 $ sudo mkdir /boot/EFI/Microsoft/
 $ sudo cp -r /mnt/EFI/Microsoft/* /boot/EFI/Microsoft/
 $ sudo umount -R /mnt
 ```

4. Create a new boot entry:
```bash
$ sudo vim /boot/loader/entries/windows.conf
```
```
title   Windows 11
efi     /EFI/Microsoft/Boot/bootmgfw.efi
```
**NOTE**: The linux ESP must be mounted on /boot and Windows EFI files ate copied to there.

5. Update boot entries
```bash
$ sudo bootctl update
```

### Set CPU frequency scaling
* See [ArchWiki](https://wiki.archlinux.org/title/CPU_frequency_scaling)

1. Using `cpupower`
* edit the file  `/etc/default/cpupower` and set:
```
governor='performance'
```
