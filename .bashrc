#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# PATHs
if [ -d "$HOME/.local/bin" ] ;
	then PATH="$HOME/.local/bin:$PATH"
fi 
alias config='/usr/bin/git --git-dir=/home/syskay/dotfiles/ --work-tree=/home/syskay'

source /home/syskay/.config/broot/launcher/bash/br
pfetch
