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
alias tf='cd ~/trackit.fit/'

ts-replace() {
  local oldString=$(printf '%s\n' "$1" | sed 's/[.[\*^$(){}?+|/]/\\&/g')
  local newString=$(printf '%s\n' "$2" | sed 's/[.[\*^$(){}?+|/]/\\&/g')

  find . -type f \( -name "*.ts" -o -name "*.tsx" \) -print0 | while IFS= read -r -d '' file; do
    # macOS (BSD) sed requires an empty string for the backup suffix to work without making backups
    if [[ "$OSTYPE" == "darwin"* ]]; then
      sed -i '' "s#$oldString#$newString#g" "$file"
    else
      sed -i "s#$oldString#$newString#g" "$file"
    fi
  done
}
