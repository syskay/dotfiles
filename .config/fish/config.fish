# PATHs
set -U fish_user_paths $HOME/.local/bin

# Welcome messages
set fish_greeting  	# Suppresses fish's welcome message
pfetch

# Aliases
alias config='/usr/bin/git --git-dir=/home/syskay/dotfiles/ --work-tree=/home/syskay'

# Promt
starship init fish | source
