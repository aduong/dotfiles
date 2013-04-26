# ls aliases
alias ll='ls -AlhF'
alias la='ls -ACF'
alias l='ls -CF'

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

alias e='emacs -nw'

# movement aliases
alias ..='cd ..'
alias ...='cd ../..'
alias -- -='cd -'

alias less='less -iS'   # case insensitive less, chop long lines
alias bc='bc -lq'       # arbitrary precision calculator
alias igrep="grep -i"   # case insensitive grep
alias egrep="grep -E"   # extended regex grep
alias rgrep="grep -r"   # recursive grep

# alias xopen='xdg-open >/dev/null 2>&1'
# alias sl='sl -e'

# alias reset-xfce4-volumed="pkill xfce4-volumed && xfce4-volumed"

alias bc='bc -lq'

alias json='python -mjson.tool'
alias uri-escape='perl -MURI::Escape -E '\''foreach (@ARGV) { say uri_escape $_ }'\'
alias uri-unescape='perl -MURI::Escape -E '\''foreach (@ARGV) { say uri_unescape $_ }'\'

test -s ~/.xfce_aliases && . ~/.xfce_aliases
test -s ~/.mac_aliases && . ~/.mac_aliases
