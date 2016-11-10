#!/bin/bash

# User dependent .bashrc file

# Source global definitions
[ -r /etc/bashrc ] && . /etc/bashrc
[ -r /etc/bash.bashrc ] && . /etc/bash.bashrc
[ -r /etc/profile ] && . /etc/profile

umask 0022

alias ssh='ssh -A -X' # Enable agent forwarding and X11 forwarding

alias sgrep='find . -regextype posix-extended -regex ".*\.(go|in|h|i|c|cc|cpp|hpp)$" | /usr/bin/xargs grep "$*"'
# Fix ssh in tmux
alias fssh='export SSH_AUTH_SOCK=$HOME/.ssh/ssh_auth_sock'

alias gitbase='git merge-base master head'
alias gitdiff='git diff $(git merge-base master head)'

# Move a file and replace original location with symbolic link 
function lmv(){ [ -e $1 -a -e $2 ] && mv $1 $2 && ln -s $2/$(basename $1) $(dirname $1); }

#alias ls='ls -B --color=tty'
# make a directory and cd into it
mkcd () 
{ 
    mkdir -p $1 && cd $1
}

## Set up history 
# Keep the last 10000 commands in the history
export HISTSIZE=10000
# Append to the bash history rather than clearing it
shopt -s histappend
# Store multilines as one
shopt -s cmdhist
# Show timestamps in history
export HISTTIMEFORMAT="%h %d %H:%M:%S> "
# Squash repeated commands into one
export HISTCONTROL=ignoredups

export PROMPT_COMMAND='history -a;'$PROMPT_COMMAND


# Force bash to check the windowsize frequently so line wrap works properly
shopt -s checkwinsize
shopt -s extglob

# EC2 CLI stuff
if [ -e ~/usr/local/ec2/ec2-api-tools-1.7.1.0 ]; then
    export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.7.1.0
    export PATH=$PATH:$EC2_HOME/bin 
fi

if [ -e ~/.aws_settings ] ; then
    . ~/.aws_settings
fi


# Go stuff
if [ -d $HOME/go ]; then
    export GOPATH=$HOME/go
    export PATH=$PATH:$GOPATH/bin
fi

# Set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
   export PATH="~/bin:${PATH}"
 fi

# Set PATH so it includes user's private bin if it exists
 if [ -d ~/scripts ] ; then
   export PATH="~/scripts:${PATH}"
 fi

if [ -d /opt/local/bin ] ; then 
    export PATH="/opt/local/bin:$PATH"
fi

if [ -d /opt/local/sbin ] ; then
    export PATH="/opt/local/sbin:$PATH"
fi

# Inlucde /usr/gnu if it exists
 if [ -d /usr/gnu ] ; then
     PATH="${PATH}:/usr/gnu/bin"
     MANPATH="${MANPATH}:/usr/gnu/man"
     INFOPATH="${INFOPATH}:/usr/gnu/info"
 fi


function ucscope {
    # Update cscope for mambo directories
    find . -name "cscope*" | xargs rm 
    find .  -name "*.[cChH]" > cscope.files
    find .  -name "*.cc" >> cscope.files
    find .  -name "*.cpp" >> cscope.files
    find .  -name "*.hpp" >> cscope.files
    find .  -name "*.js" >> cscope.files
    cscope -b -q -k
}

export EDITOR=emacs
export VISUAL=emacs

### Stuff for GNU Global
export LESSGLOBALTAGS=global


# Local specific commands go in another file
if [ -e ~/.bash_local ] ; then
    . ~/.bash_local
fi

