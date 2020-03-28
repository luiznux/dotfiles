   █████╗ ██████╗  ██████╗██╗  ██╗     ██████╗ ██╗   ██╗██╗██████╗ ███████╗
  ██╔══██╗██╔══██╗██╔════╝██║  ██║    ██╔════╝ ██║   ██║██║██╔══██╗██╔════╝
  ███████║██████╔╝██║     ███████║    ██║  ███╗██║   ██║██║██║  ██║█████╗
  ██╔══██║██╔══██╗██║     ██╔══██║    ██║   ██║██║   ██║██║██║  ██║██╔══╝
  ██║  ██║██║  ██║╚██████╗██║  ██║    ╚██████╔╝╚██████╔╝██║██████╔╝███████╗
  ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝     ╚═════╝  ╚═════╝ ╚═╝╚═════╝ ╚══════╝





1. Set keyboard layout

``` bash
$ loadkeys br-abnt2
```


2. Load crypt modules

```bash
$ modprobe -a dm-mod dm-crypt
```


3. Create particions

```bash
$ fdisk -l && cfdisk /dev/sdX
```

|/dev/sda1 |--> |BIOS BOOT EFI 500MB size(FAT32 TYPE)|
|/dev/sda2 |--> |BOOT  500MB|
|/dev/sda3 |--> |LINUX LVM TYPE any size that you want|


4. Encrypt the "LINUX LVM" particion

```bash
$ cryptsetup -y -v luksFormat --type luks1 -c aes-xts-plain64 -s 512 /dev/sda3
```

..* just remember that a password will be required


5. Open your crypt

```
$ cryptsetup open  --type luks /dev/sda3 linux
```


6. PV linux

* to see info about Physical Volume (PV)

```
$ pvs
```

* create one

```
$ pvcreate /dev/mapper/linux
```


7. Create VG(Volume Group)

```
$ vgcreate linux /dev/mapper/linux
```

8. Create a LV(Logical Volume)

```
$ lvcreate -L 1G linux -n swap
```

* to see lv

```
$ lvs
```

9. Create the other two LV, "home" and "/"

```
$ lvcreate -L 30G linux -n archlinux
$ lvcreate -l +100%FREE linux -n home
```

* activate volumes
```
$ vgchange -ay
```

10. Format particions

* BOOT , crypt

```
$ mkfs.fat -F32 /dev/sda1
$ mkfs.ext4 /dev/sda2
$ mkfs -t ext4 /dev/mapper/linux-archlinux
$ mkfs -t ext4 /dev/mapper/linux-home
$ mkswap /dev/mapper/linux-swap
$ swapon /dev/mapper/linux-swap
$ lsblk -f
```

11. Mount particions

```
$ mount /dev/mapper/linux-archlinux /mnt
$ mkdir /mnt/home
$ mount /dev/mapper/linux-home /mnt/home

$ mkdir /mnt/boot/
$ mount /dev/sda2 /mnt/boot

$ mkdir /mnt/boot/efi
$ mount /dev/sda1 /mnt/boot/efi
```

(12) Packages

* edit mirror list

```
$ vim /etc/pacman.d/mirrorlist
```

* Install the base packages and genfstab

```
$ pacstrap -i /mnt base-devel base linux linux-firmware

$ genfstab -U -p /mnt >> /mnt/etc/fstab
```

13. Mv chroot to /mnt

```
$ arch-chroot /mnt /bin/bash
```

14. Install some packages

```
$ pacman -S bash-completion sudo os-prober wireless_tools networkmanager  network-manager-applet mtools vim  wpa_supplicant dosfstools  dialog lvm2  linux-headers ntfs-3g --noconfirm
```


15. Set locale

```
$ rm -f /etc/localtime
$ ln -s /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime
$ locale-gen
$ echo KEYMAP=br-abnt2 >> /etc/vconsole.conf
```

16. Host and users

```bash
$ echo "arch" > /etc/hostname
$ passwd
```

* Add a user
```
$ useradd -m -g users -G wheel,games,power,optical,storage,scanner,lp,audio,video -s /bin/bash luiznux

$ passwd luiznux

$ echo "luiznux ALL=(ALL)ALL" >> /etc/sudoers
```


17. mkinitcpio.conf

```
$ vim /etc/mkinitcpio.conf
```

* look for HOOKS="...." and add:

```
HOOKS=(base udev autodetect keyboard keymap consolefont modconf block lvm2 encrypt filesystems fsck)
```
* ADD THIS AT THE SAME ORDER
* safe and exit and reload

```
$ mkinitcpio -p linux
```


18. GRUB

* USING UEFI MODE

1. INSTALL
```
$ pacman -S grub efibootmgr --noconfirm
```

2. CONFIG

```
$ vim /etc/default/grub
```

*look for   GRUB_CMDLINE_LINUX=”“, and the set:

```
GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=/dev/sda3:linux:allow-discards quiet splash pci=nomsi"
```

3. after that, write or update those variables to:
```
GRUB_PRELOAD_MODULES="lvm..."
GRUB_ENABLE_CRYPTODISK=y
GRUB_DISABLE_SUBMENU=y
```

4. INSTALLING

```
$ grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub --recheck

$ cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
```


5. Gen grub file
```
$ grub-mkconfig -o /boot/grub/grub.cfg
```

19. Exit, be happy and pray for the grub to work :)

```
$ exit
$ umount -R /mnt
$ systemctl reboot

$ systemctl enable NetworkManager
$ systemctl enable dhcpcd
```
