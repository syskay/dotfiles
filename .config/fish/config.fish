# PATHs
set -U fish_user_paths $HOME/.local/bin

# VARIABLES
set -gx TERMINAL "st"
set -gx EDITOR "nvim"

setxkbmap pl

# Welcome messages
set fish_greeting  	# Suppresses fish's welcome message
pfetch

# Aliases
alias config='/usr/bin/git --git-dir=/home/syskay/dotfiles/ --work-tree=/home/syskay'
alias ll='exa -l'
alias la='exa -la'

# Promt
starship init fish | source
