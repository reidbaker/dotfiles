# Path to your oh-my-zsh configuration.
ZSH=$HOME/Documents/Programs/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="blinks"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

## Fun commands ##
##################
# Thanks to http://coderwall.com/p/hvfvva
alias fact="elinks -dump randomfunfacts.com | sed -n '/^|/p' | tr -d \|"

## git commands ##
##################
alias gd="git diff"

## System commands ##
#####################
alias apt-s='apt-cache search'
alias la='ls -alh'
alias emacskilld="emacsclient -e '(kill-emacs)'"
alias ack='ack-grep'
# use | tr -d '\n' | to strip new lines
alias cboard='xclip -selection clipboard' # allows piping into pastebuffer

## Project commands ##
######################
alias cddotfiles='pushd ~/.dotfiles/'

# Turn on caching, which helps with e.g. apt
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Ignore some common useless files
zstyle ':completion:*' ignored-patterns '*?.pyc' '__pycache__'
zstyle ':completion:*:*:rm:*:*' ignored-patterns

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

## History ##
#############
setopt extended_history hist_no_store hist_ignore_dups hist_expire_dups_first hist_find_no_dups inc_append_history share_history hist_reduce_blanks hist_ignore_space
export HISTFILE=~/.zsh_history
export HISTSIZE=1000000
export SAVEHIST=1000000

# Up/down arrow.
# I want shared history for ^R, but I don't want another shell's activity to
# mess with up/down.  This does that.
down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history
up-line-or-local-history() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N up-line-or-local-history

bindkey "\e[A" up-line-or-local-history
bindkey "\e[B" down-line-or-local-history


# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git command-not-found pip virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# Set emacs as the editor # This isnt working TODO fix
export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient

# Sbt for scala
PATH=$PATH:$HOME/Documents/hacking/sbt/bin/
