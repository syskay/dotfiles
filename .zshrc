# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh/histfile
HISTSIZE=100000
SAVEHIST=100000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/syskay/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Promnt
eval "$(starship init zsh)"


# Enabl colors
autoload -U colors && colors

# Enable auto complete
autoload -U compinit
zstyle ':completetion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# Aliases
alias config='/usr/bin/git --git-dir=/home/syskay/dotfiles/ --work-tree=/home/syskay'

alias ll='exa -l'
alias la='exa -la'

pfetch
