# User dependent .bashrc file

umask 0022

alias ssh='ssh -A -X' # Enable agent forwarding and X11 forwarding

# Move a file and replace original location with symbolic link 
function lmv(){ [ -e $1 -a -e $2 ] && mv $1 $2 && ln -s $2/$(basename $1) $(dirname $1); }

#alias ls='ls -B --color=tty'
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

export JAVA_HOME=$(/usr/libexec/java_home)
# EC2 CLI stuff
export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.7.1.0
export PATH=$PATH:$EC2_HOME/bin 
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


# Local specific commands go in another file
if [ -e ~/.bash_local ] ; then
    . ~/.bash_local
fi
