#!/bin/sh
xrandr -s 1680x1050
nitrogen --set-scaled ~/wallpapers/tree_water-3840x2160.png
[[ -f ~/.Xresources ]]  && xrdb -merge -I$HOME ~/.Xresources
picom &
dwmblocks &
exec dwm
