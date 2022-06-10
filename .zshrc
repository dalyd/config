
# Created by `pipx` on 2022-03-28 19:06:26
export PATH="$PATH:/Users/david.daly/.local/bin"

# Enable zsh-autocomplete
source ~/zshstuff/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

umask 0022

alias ssh='ssh -A -YC' # Enable agent forwarding and X11 forwarding

# Fix ssh in tmux
alias fssh='export SSH_AUTH_SOCK=$HOME/.ssh/ssh_auth_sock'

alias gitbase='git merge-base master HEAD'
alias gitdiff='git diff $(gitbase)'

# Move a file and replace original location with symbolic link 
function lmv(){ [ -e "$1" ] && [ -e "$2" ] && mv "$1" "$2" && ln -s "$2" $"(basename $1)" $"(dirname $1)"; }

#alias ls='ls -B --color=tty'
# make a directory and cd into it
mkcd () 
{ 
    mkdir -p "$1" && cd "$1" || exit
}

# shellcheck source=/USERS/daviddaly/bin/git-completion.bash
if [ -f "${HOME}"/bin/git-completion.bash ]; then
  source "${HOME}"/bin/git-completion.bash
fi

## Set up history 
export HISTFILE=~/.zsh_history
# Keep the last 10000 commands in the history
export HISTFILESIZE=10000
export HISTSIZE=10000
export SAVEHIST=10000
# # Append to the bash history rather than clearing it
# shopt -s histappend
# # Store multilines as one
# shopt -s cmdhist
# Show timestamps in history
export HISTTIMEFORMAT="%h %d %H:%M:%S> "
# Squash repeated commands into one
export HISTCONTROL=ignoredups

export PROMPT_COMMAND='history -a;'$PROMPT_COMMAND


# # Force bash to check the windowsize frequently so line wrap works properly
# shopt -s checkwinsize
# shopt -s extglob

# EC2 CLI stuff
if [ -e "${HOME}"/usr/local/ec2/ec2-api-tools-1.7.1.0 ]; then
    export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.7.1.0
    export PATH=$PATH:$EC2_HOME/bin 
fi

# shellcheck source=/dev/null
if [ -e "${HOME}"/.aws_settings ] ; then
    . "${HOME}"/.aws_settings
fi


# Go stuff
if [ -d "${HOME}"/go ]; then
    export GOPATH=$HOME/go
    export PATH=$PATH:$GOPATH/bin
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}"/bin ] ; then
   export PATH="${HOME}/bin:${PATH}"
 fi

# Set PATH so it includes user's private bin if it exists
 if [ -d "${HOME}"/scripts ] ; then
   export PATH="${HOME}/scripts:${PATH}"
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

export EDITOR=emacs
export VISUAL=emacs

### Stuff for GNU Global
export LESSGLOBALTAGS=global
eval "$(starship init zsh)"

fpath=(~/.zsh $fpath)
fpath=(/Users/david.daly/zshstuff/zsh-completions/src $fpath)
complete -f -W "infrastructure_provisioning mongodb_setup workload_setup test_control infrastructure_teardown conn" run-dsi

eval $(thefuck --alias)
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
