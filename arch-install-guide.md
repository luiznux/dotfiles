```ascii
 █████╗ ██████╗  ██████╗██╗  ██╗
██╔══██╗██╔══██╗██╔════╝██║  ██║
███████║██████╔╝██║     ███████║
██╔══██║██╔══██╗██║     ██╔══██║
██║  ██║██║  ██║╚██████╗██║  ██║
╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝
```

# Arch linux Install guide
This archive contains some of instructions for install arch linux distro.
Remembering that all the steps that I will take can be changed at your discretion
without any problem. The great advantage of this distro is that each one adapts the
process to their needs, so feel free to change anything.
The following steps serve more as an example of how I do my installation, so your best
friend will be the [ArchWiki](https://wiki.archlinux.org/), where 99% of your problems
and doubts will be there.

# My config
My configuration is not very sophisticated, I just partition with a separate home
directory and two other boot partitions. In addition, I use LUKS encryption, together
with an LVM (Logical Volume Management), so the data is organized and secure.
The figure below illustrates how my partitioning is configured.

```
|----------------|-----------------------------------------------------------------------------------------|
|   /dev/sda1    |                                   /dev/sda2                                             |
|----------------|-----------------------------------------------------------------------------------------|
| Boot partition | dm-crypt plain encrypted volume | LUKS2 encrypted volume    | LUKS2 encrypted volume    |
|                |                                 |                           |                           |
| /boot          | [SWAP]                          | /                         | /home                     |
|                |                                 |                           |                           |
|                | /dev/mapper/swap                | /dev/mapper/root          | /dev/mapper/home          |
|                | --------------------------------| --------------------------| --------------------------|
|                | Logical volume 1                | Logical volume 2          | Logical volume 3          |
|                | /dev/MyVolGroup/cryptswap       | /dev/MyVolGroup/cryptroot | /dev/MyVolGroup/crypthome |
|----------------|---------------------------------|---------------------------|---------------------------|
```

**Font**  https://wiki.archlinux.org/index.php/Dm-crypt/Encrypting_an_entire_system#LUKS_on_LVM

So lets get started !


## 1. Set keyboard layout

``` bash
$ loadkeys br-abnt2
```

## 2. Load crypt modules

```bash
$ modprobe -a dm-mod dm-crypt
```


## 3. Create particions

```bash
$ fdisk -l && cfdisk /dev/sdX
```
|Device       |Type           | Size     | Format   |
| ----------- |:-------------:|:--------:|:--------:|
| `/dev/sda1` | BIOS BOOT EFI | 500MB    |FAT32     |
| `/dev/sda2` | BOOT          | 500MB    |EXT4      |
| `/dev/sda3` | LINUX LVM     |  any     |EXT4      |


## 4. Encrypt the "LINUX LVM" particion

```bash
$ cryptsetup -y -v luksFormat --type luks1 -c aes-xts-plain64 -s 512 /dev/sda3
```

* just remember that a password will be required


## 5. Open your crypt

```bash
$ cryptsetup open  --type luks /dev/sda3 linux
```

## 6. PV linux


* to see info about Physical Volume (PV)

```bash
$ pvs
```

* create one

```bash
$ pvcreate /dev/mapper/linux
```


## 7. Create Volume Group (VG)


```bash
$ vgcreate linux /dev/mapper/linux
```


## 8. Create a Logical Volume (LV)

```bash
$ lvcreate -L 1G linux -n swap
```

* to see lv

```bash
$ lvs
```

## 9. Create the other two LV, "home" and "/"

dasds
```bash
$ lvcreate -L 30G linux -n archlinux

$ lvcreate -l +100%FREE linux -n home
```

* activate the volumes

```bash
$ vgchange -ay
```

## 10. Format particions


* BOOT, crypt

```bash
$ mkfs.fat -F32 /dev/sda1
$ mkfs.ext4 /dev/sda2

$ mkfs -t ext4 /dev/mapper/linux-archlinux
$ mkfs -t ext4 /dev/mapper/linux-home

$ mkswap /dev/mapper/linux-swap
$ swapon /dev/mapper/linux-swap
$ lsblk -f
```

## 11. Mount particions


```bash

$ mount /dev/mapper/linux-archlinux /mnt
$ mkdir /mnt/home
$ mount /dev/mapper/linux-home /mnt/home

$ mkdir /mnt/boot/
$ mount /dev/sda2 /mnt/boot

$ mkdir /mnt/boot/efi
$ mount /dev/sda1 /mnt/boot/efi
```


## 12. Packages

* edit mirror list

```bash
$ vim /etc/pacman.d/mirrorlist
```

* Install the base packages and genfstab

```bash
$ pacstrap -i /mnt base-devel base linux linux-firmware

$ genfstab -U -p /mnt >> /mnt/etc/fstab
```


## 13. Mv chroot to /mnt


```bash
$ arch-chroot /mnt /bin/bash
```


## 14. Install some packages

```bash
$ pacman -S bash-completion sudo os-prober wireless_tools networkmanager  network-manager-applet mtools vim  wpa_supplicant dosfstools  dialog lvm2  linux-headers ntfs-3g --noconfirm
```

## 15. Set locale

```bash
$ rm -f /etc/localtime
$ ln -s /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime

$ locale-gen
$ echo KEYMAP=br-abnt2 >> /etc/vconsole.conf
```


## 16. Host and users

```bash
$ echo "arch" > /etc/hostname

$ passwd
```

* Add a user

```bash
$ useradd -m -g users -G wheel,games,power,optical,storage,scanner,lp,audio,video -s /bin/bash luiznux

$ passwd luiznux

$ echo "luiznux ALL=(ALL)ALL" >> /etc/sudoers
```


## 17. mkinitcpio.conf

```bash
$ vim /etc/mkinitcpio.conf
```

* look for HOOKS="...." and add:

```bash
HOOKS=(base udev autodetect keyboard keymap consolefont modconf block lvm2 encrypt filesystems fsck)
```
* ADD THIS AT THE SAME ORDER
* safe and exit and reload

```bash
$ mkinitcpio -p linux
```


## 18. GRUB

* USING UEFI MODE

1. INSTALL
```bash
$ pacman -S grub efibootmgr --noconfirm
```

2. CONFIG

```bash
$ vim /etc/default/grub
```

* look for   GRUB_CMDLINE_LINUX=”“, and the set:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=/dev/sda3:linux:allow-discards quiet splash pci=nomsi"
```

3. after that, write or update those variables to:
```bash
GRUB_PRELOAD_MODULES="lvm..."

GRUB_ENABLE_CRYPTODISK=y

GRUB_DISABLE_SUBMENU=y
```

4. INSTALLING

```bash
$ grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub --recheck

$ cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
```


5. Gen grub file

```bash
$ grub-mkconfig -o /boot/grub/grub.cfg
```

## 19. Exit, be happy and pray for the grub to work :)


```bash
$ exit
$ umount -R /mnt
$ systemctl reboot

$ systemctl enable NetworkManager
$ systemctl enable dhcpcd
```
