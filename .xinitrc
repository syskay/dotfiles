#!/bin/sh
xrandr -s 1680x1050
nitrogen --set-scaled ~/wallpapers/wallhaven-76q6ve_1680x1050.png
[[ -f ~/.Xresources ]]  && xrdb -merge -I$HOME ~/.Xresources
picom &
dwmblocks &
exec dwm
