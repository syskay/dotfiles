#!/bin/sh
xrandr -s 1680x1050
nitrogen --set-scaled ~/wallpapers/tree_water-3840x2160.png
picom &
dwmblocks &
exec dwm
