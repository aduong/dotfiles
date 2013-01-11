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

# alias em='emacs -nw'
# alias et='emclient'

# movement aliases
alias ..='cd ..'
alias ...='cd ../..'
alias -- -='cd -'

alias less='less -iS'   # case insensitive less, chop long lines
alias bc='bc -lq'       # arbitrary precision calculator
alias igrep="grep -i"   # case insensitive grep

# alias xopen='xdg-open >/dev/null 2>&1'
# alias sl='sl -e'

# alias reset-xfce4-volumed="pkill xfce4-volumed && xfce4-volumed"

alias bc='bc -lq'

test -s ~/.xfce_aliases && . ~/.xfce_aliases
