#
#  ███████╗███████╗██╗  ██╗██████╗  ██████╗
#  ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝
#    ███╔╝ ███████╗███████║██████╔╝██║
#   ███╔╝  ╚════██║██╔══██║██╔══██╗██║
#  ███████╗███████║██║  ██║██║  ██║╚██████╗
#  ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝
#
#

#---------------------------------#
#------------ Exports ------------#
#
export EDITOR=vim
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export TERM="xterm-256color"  # getting proper colors
export GOPATH=$HOME/go
export LSP_USE_PLISTS=true

#PATH
export PATH="${PATH}:${HOME}/.local/bin:${HOME}/.config/scripts:$GOPATH/bin"

#---------------------------------#
#------------ Aliases-------------#
#
## Shortcuts
alias v='vim'
alias sv='sudo vim'
alias r='ranger'
alias o='xdg-open'
alias ka='killall'
alias SS='sudo systemctl'
alias ..='cd ..'
alias ...='cd ../../../'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

## Changing "ls" to "exa"
#alias ls='exa -alF --color=always --group-directories-first' # my preferred listing
#alias la='exa -a --color=always --group-directories-first'  # all files and dirs
#alias ll='exa -l --color=always --group-directories-first'  # long format
#alias lt='exa -aT --color=always --group-directories-first' # tree listing
#alias l.='exa -a | egrep "^\."'

## Pacman
alias p='sudo pacman'
alias pc='sudo pacman -Scc'                      # clean all pacman cache
alias update='sudo pacman -Syu'                  # update all packages(Archlinux)
alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
alias cleanup='sudo pacman -R $(pacman -Qtdq)'   # remove orphaned packages
alias yaysua='yay -Sua --noconfirm'              # update only AUR pkgs (yay)

## Monitors
alias fans='sensors | grep -i fan'
alias wfans="watch -n1 -d 'sensors | grep fan'"

## audio
alias sample='pacmd list-sinks | grep "sample spec"'

## Networks
alias wifi-list='nmcli device wifi list' # list wifi networks
alias wifi-on='nmcli r wifi on'          # enable wifi
alias wifi-off='nmcli r wifi off'        # disable wifi
alias ethspeed='speedometer -r enp2s0'
alias wifispeed='speedometer -r wlp3s0'

## My configs
alias i3conf='vim ~/.config/i3/config'
alias polyconf='vim ~/.config/polybar/config.ini'
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"

## Adding flags
alias df='df -h'     # human-readable sizes
alias free='free -m' # show sizes in MB

## Ps
alias psa="ps auxf"
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"
alias psmem='ps auxf | sort -nr -k 4'
alias pscpu='ps auxf | sort -nr -k 3'

## Utilities
alias font-update='sudo fc-cache -fv'
alias diff=colordiff
alias jctl="journalctl -p 3 -xb"  # get error messages from journalctl

## Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'


#---------------------------------#
#---------- zsh configs ----------#
#
## history  cache
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000

## If not running interactively, don't do anything
[[ $- != *i* ]] && return

## enable color support of ls and also add handy aliases
##if [ -x /usr/bin/dircolors ]; then
##    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
##    alias ls='ls --color=auto'
##    #alias dir='dir --color=auto'
##    #alias vdir='vdir --color=auto'
##
##    alias grep='grep --color=auto'
##    alias fgrep='fgrep --color=auto'
##    alias egrep='egrep --color=auto'
##fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


#---------------------------------#
#--------- Vim mode zsh ----------#
#
bindkey -v

## no delay entering normal mode
## https://coderwall.com/p/h63etq
## https://github.com/pda/dotzsh/blob/master/keyboard.zsh#L10
## 10ms for key sequences
export KEYTIMEOUT=1

## auto cd into typed directory
setopt autocd

setopt nomatch notify
unsetopt autocd beep extendedglob

## autocomplete config
setopt globdots
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
_comp_options+=(globdots)	#include hidden files
## vim keys to navigation on menu completion
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history


#---------------------------------#
#----------- Oh My Zsh -----------#
#
# If you come from bash you might have to change your $PATH.
#export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/$USER/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
 HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

ZSH_DISABLE_COMPFIX="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    ssh-agent
    colorize
    colored-man-pages
    extract
    #vi-mode
    history
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-fzf-history-search
)

zstyle :omz:plugins:ssh-agent identities ~/.ssh/*_rsa

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#

#---------------------------------#
#---------- Emacs Vterm ----------#
#
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# vterm-clear-scrollback
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# vterm-buffer-name-string
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}

# Directory tracking and Prompt tracking
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
