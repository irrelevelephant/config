#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
  exec sway
fi

export LANG=en_US.UTF-8
alias EDITOR=mg
alias emacs='emacs -nw'

source /usr/share/nvm/init-nvm.sh
alias chromium='chromium --enable-features=UseOzonePlatform --ozone-platform=wayland'
alias tf='cd ~/trackit.fit/apps/website'
