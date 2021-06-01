#!/bin/sh
xrandr -s 1680x1050
nitrogen --set-scaled ~/wallpapers/famtasy-5701x3899.jpg
[[ -f ~/.Xresources ]]  && xrdb -merge -I$HOME ~/.Xresources
picom &
dunst &
dwmblocks &
exec dwm
#exec qtile
