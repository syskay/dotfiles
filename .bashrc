#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# VARIABLES
export TERMINAL="st"
export EDITOR="nvim"

# PATHs
if [ -d "$HOME/.local/bin" ] ;
	then PATH="$HOME/.local/bin:$PATH"
fi 
alias config='/usr/bin/git --git-dir=/home/syskay/dotfiles/ --work-tree=/home/syskay'

setxkbmap pl

#source /home/syskay/.config/broot/launcher/bash/br
#pfetch

#powerline-daemon -q
#POWERLINE_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. /usr/share/powerline/bindings/bash/powerline.sh

force_color_promt=yes

eval "$(starship init bash)"
