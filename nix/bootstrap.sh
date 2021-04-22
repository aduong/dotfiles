#!/usr/bin/env bash
#
# Bootstraps NixOS on a system. Creates a new GPT partition table with an EFI system partition and a LUKS-encrypted
# partition. Creates an LVM volume group with a swap and root volume within the encrypted partition. Also creates a
# keyfile that's embedded in the initramfs in the encrypted partition so the encryption password only has to be entered
# once on boot.
#
# The commands are meant to be run command by command from a live OS (CD or USB drive).

# This should be changed to suit the system
device=/dev/sda

# Clear the existing partition table
sgdisk --clear $device

# Create an EFI system partition (ESP) as the first partition
efi_part_num=1
sgdisk --new=${efi_part_num}:+0:+550M --typecode=${efi_part_num}:ef00 --change-name=${efi_part_num}:ESP $device

# Create the LUKS encrypted partition as the second partition
root_part_num=2
sgdisk --largest-new=${root_part_num} --typecode=${root_part_num}:8309 --change-name=${root_part_num}:Encrypted $device

# Print to check on the partiiton table
sgdisk --print $device

# Set up the ESP as fat32
mkfs.fat -F32 -n boot ${device}${efi_part_num}

# Set up the LUKS-encrypted partition. Create a keyfile that can be included in the initramfs so that
# the password is entered in a single time. On boot, GRUB should mount the encrypted volume after entering
# the password and the initramfs will reside in the encrypted volume. The keyfile should be specified in
# the boot.initrd.secrets NixOS configuration attribute as well as the boot.initrd.luks.devices one.
keyfilename=crypto_keyfile.bin
umask 0077 # let's be a bit careful here
head -c4K /dev/urandom > $keyfilename
luks_mapping=cryptlvm
# GRUB mainline supports LUKS2 but the latest release at the time (2.04, July 2019) doesn't.
cryptsetup luksFormat --type luks1 ${device}${root_part_num} $keyfilename
# Set up the encryption password. This is done after the keyfile so that the password only needs to be
# entered once (plus verification) during setup.
cryptsetup luksAddKey --key-file $keyfilename ${device}${root_part_num}
cryptsetup open --key-file $keyfilename ${device}${root_part_num} $luks_mapping

# Create a volume group on the LUKS-encrypted partition
vol_group=vgnix
pvcreate /dev/mapper/${luks_mapping}
vgcreate $vol_group /dev/mapper/${luks_mapping}

# Create a small logical volume for swap and the rest goes to the root FS
lvcreate $vol_group -L 1G -n swap
lvcreate $vol_group -l 100%FREE -n root

# Format the swap and root partitions
mkswap -L swap /dev/${vol_group}/swap
mkfs.ext4 /dev/${vol_group}/root

# Mount everything in preparation for installation
swapon /dev/${vol_group}/swap
mount /dev/${vol_group}/root /mnt
mkdir -p /mnt/boot/efi
mount ${device}${efi_part_num} /mnt/boot/efi
# This is where we copy the keyfile made from earlier
mkdir -p /mnt/boot/initrd
mv $keyfilename /mnt/boot/initrd/${keyfilename}

# Generate the NixOS configs
nixos-generate-config --root /mnt

# Edit the NixOS configs or copy them from somewhere...
vi /mnt/etc/nixos/configuration.nix

# Install
nixos-install

# Reboot
reboot
