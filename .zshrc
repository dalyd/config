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

# Starship prompt (disable inside Claude Code to avoid rendering issues)
if [[ -z "$CLAUDECODE" ]]; then
    eval "$(starship init zsh)"
fi

# Completions
fpath=($(brew --prefix)/share/zsh-completions ~/.zsh $fpath)
autoload -Uz compinit
compinit -u

# iTerm2 integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Claude Code with Emacs MCP integration
function claude() {
    local project_dir="$(pwd)/"
    local session_id
    session_id=$(emacsclient --eval "(my/claude-code-register-project \"$project_dir\")" 2>/dev/null | tr -d '"')
    if [[ -n "$session_id" ]]; then
        command claude --mcp-config "{\"mcpServers\":{\"emacs-tools\":{\"type\":\"http\",\"url\":\"http://localhost:21567/mcp/$session_id\"}}}" "$@"
    else
        echo "Warning: Could not register project with Emacs MCP server (is Emacs running?)"
        command claude "$@"
    fi
}

# Machine-specific overrides (symlinked from profiles/)
source ~/.zshrc.local 2>/dev/null

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/daviddaly/.lmstudio/bin"
# End of LM Studio CLI section

