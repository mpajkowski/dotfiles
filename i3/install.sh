#!/bin/bash

set -e

yes | LC_ALL=en_US.UTF-8 sudo pacman --noconfirm -Syu i3 picom i3status-rust arandr

mkdir -p $HOME/.config/i3
mkdir -p $HOME/.config/i3status-rust

I3CONFIG_DEST="$HOME/.config/i3/config"
PICOM_DEST="$HOME/.config/picom.conf"
I3STATUS_DEST="$HOME/.config/i3status-rust"

ln -s --backup=numbered "$PWD/i3_config" $HOME/.config/i3/config
ln -s --backup=numbered "$PWD/picom.conf" $HOME/.config/picom.conf
ln -s --backup=numbered "$PWD/config.toml" $HOME/.config/i3status-rust/config.toml
