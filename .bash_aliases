## git commands ##
##################
alias gb='git branch'
alias gba='git branch -a'
alias gst='git status'
alias gco='git checkout'
alias gd='git diff'

## system commands ##
#####################
alias apt-s='apt-cache search'
alias la='ls -alh'
alias emacskilld="emacsclient -e '(kill-emacs)'"
# mid 15 20 file.txt prints out lines 15-20 inclusive
function mid() { awk NR==$1,NR==$2 $3 ;}
alias ack='ack-grep'

## project commands ##
######################
alias gotoetest='cd /home/reid/Documents/work/techTeam/etest'
alias gotomatlab='cd /home/reid/Documents/MATLAB'
alias gotodotfiles='cd /home/reid/.dotfiles'
alias gotoresume='cd /home/reid/Documents/interviewStuff/resume'
