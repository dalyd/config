# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Zsh plugins
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

umask 0022

# Aliases
alias ssh='ssh -A -YC' # Enable agent forwarding and X11 forwarding
alias fssh='export SSH_AUTH_SOCK=$HOME/.ssh/ssh_auth_sock' # Fix ssh in tmux
alias gitbase='git merge-base master HEAD'
alias gitdiff='git diff $(gitbase)'

# Move a file and replace original location with symbolic link
function lmv(){ [ -e "$1" ] && [ -e "$2" ] && mv "$1" "$2" && ln -s "$2" $"(basename $1)" $"(dirname $1)"; }

# Make a directory and cd into it
mkcd () {
    mkdir -p "$1" && cd "$1" || return
}

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
eval "$(pyenv init -)"

## History
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt SHARE_HISTORY

# Go
if [ -d "${HOME}"/go ]; then
    export GOPATH=$HOME/go
    export PATH=$PATH:$GOPATH/bin
fi

# User bin directories
if [ -d "${HOME}"/bin ]; then
    export PATH="${HOME}/bin:${PATH}"
fi

if [ -d "${HOME}"/scripts ]; then
    export PATH="${HOME}/scripts:${PATH}"
fi

if [ -d "${HOME}"/.local/bin ]; then
    export PATH="${HOME}/.local/bin:${PATH}"
fi

export EDITOR=emacs
export VISUAL=emacs

# Starship prompt
eval "$(starship init zsh)"

# Completions
fpath=($(brew --prefix)/share/zsh-completions ~/.zsh $fpath)
autoload -Uz compinit
compinit

# iTerm2 integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Machine-specific overrides (symlinked from profiles/)
source ~/.zshrc.local 2>/dev/null
