# ls aliases
alias lt='ls -l -t --almost-all --classify --human-readable --reverse'
alias ll='ls -l --almost-all --classify --human-readable'
alias la='ls -C --almost-all --classify'
alias l='ls -C --classify'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
	test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
	alias ls='ls --color=auto'

	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
fi

# movement aliases
alias ..='cd ..'
alias ...='cd ../..'
alias -- -='cd -'

alias less='less -iS'   # case insensitive less, chop long lines
alias bc='bc -lq'       # arbitrary precision calculator
alias igrep="grep -i"   # case insensitive grep
alias egrep="grep -E"   # extended regex grep
alias rgrep="grep -r"   # recursive grep

alias rm="rm -I"        # safe rm

alias sl='sl -e'

# alias reset-xfce4-volumed="pkill xfce4-volumed && xfce4-volumed"

alias json='python -mjson.tool'
alias server='python -mSimpleHTTPServer'
alias uri-escape='perl -MURI::Escape -E '\''foreach (@ARGV) { say uri_escape $_ }'\'
alias uri-unescape='perl -MURI::Escape -E '\''foreach (@ARGV) { say uri_unescape $_ }'\'

alias cbcopy='xsel --clipboard -i'
alias cbpaste='xsel --clipboard -o'

test -s ~/.xfce_aliases && . ~/.xfce_aliases

alias ec='emacsclient'
