# /$$$$$$$                      /$$
#| $$__  $$                    | $$
#| $$  \ $$  /$$$$$$   /$$$$$$$| $$$$$$$   /$$$$$$   /$$$$$$$
#| $$$$$$$  |____  $$ /$$_____/| $$__  $$ /$$__  $$ /$$_____/
#| $$__  $$  /$$$$$$$|  $$$$$$ | $$  \ $$| $$  \__/| $$
#| $$  \ $$ /$$__  $$ \____  $$| $$  | $$| $$      | $$
#| $$$$$$$/|  $$$$$$$ /$$$$$$$/| $$  | $$| $$      |  $$$$$$$
#|_______/  \_______/|_______/ |__/  |__/|__/       \_______/
#
#
#
#


export EDITOR=vim

#---------------------------------#
#------------ Aliases-------------#
#
alias   r='ranger'
alias  ..='cd ..'
alias ...='cd ../../../'
alias wifi-list='nmcli device wifi list' #list wifi networks
alias wifi-on='nmcli r wifi on' #enable wifi
alias wifi-off='nmcli r wifi off' # disable wifi
alias i3conf="vim ~/.config/i3/config"
alias polyconf="vim ~/.config/polybar/config"
alias update='sudo pacman -Syu' # update all packages(Archlinux)

#---------------------------------#
#----------- default -------------#
#
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#-------------------------------------#
#-------------Powerline --------------#
#
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/share/powerline/bindings/bash/powerline.sh
