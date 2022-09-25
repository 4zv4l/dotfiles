#!/bin/bash

setxkbmap be
# key shortcut
sxhkd -c $HOME/.config/berry/sxhkdrc &
# notification
dunst &
# wallpaper
~/.fehbg &
# effect
picom --config $HOME/.config/picom/picom.conf &
/etc/X11/xinit/xinitrc.d/40-libcanberra-gtk-module.sh
/etc/X11/xinit/xinitrc.d/50-systemd-user.sh
exec berry
