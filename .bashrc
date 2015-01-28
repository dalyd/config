# User dependent .bashrc file

export PATH=/usr/local/bin:/sw/bin:/sw/sbin:/opt/local/bin:/opt/local/sbin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin

umask 0022

alias ssh='ssh -A -X' # Enable agent forwarding and X11 forwarding

function lmv(){ [ -e $1 -a -e $2 ] && mv $1 $2 && ln -s $2/$(basename $1) $(dirname $1); }

#alias ls='ls -B --color=tty'
alias cresults="cd ~/Google\ Drive/experiments/"
alias nresults="mkdir -p ~/Google\ Drive/experiments/$(date +%Y-%m-%d)"
alias tresults="cd ~/Google\ Drive/experiments/$(date +%Y-%m-%d)"
# make a directory and cd into it
mkcd () 
{ 
    mkdir -p $1 && cd $1
}

## Set up history 
# Keep the last 2000 commands in the history
export HISTSIZE=2000
# Append to the bash history rather than clearing it
shopt -s histappend
# Force bash to check the windowsize frequently so line wrap works properly
shopt -s checkwinsize
shopt -s extglob

# redundant
# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
#PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
#export PATH

export JAVA_HOME=$(/usr/libexec/java_home)
# EC2 CLI stuff
export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.7.1.0
export PATH=$PATH:$EC2_HOME/bin 
if [ -e ~/.aws_settings ] ; then
    . ~/.aws_settings
fi


# Go stuff
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin

# Set PATH so it includes user's private bin if it exists
 if [ -d ~/bin ] ; then
   PATH="~/bin:${PATH}"
 fi

# Set PATH so it includes user's private bin if it exists
 if [ -d ~/scripts ] ; then
   PATH="~/scripts:${PATH}"
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

export EDITOR=xemacs
export VISUAL=xemacs


# Local specific commands go in another file
if [ -e ~/.bash_local ] ; then
    . ~/.bash_local
fi
