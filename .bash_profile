
test -r /sw/bin/init.sh && . /sw/bin/init.sh

if [ -e ~/.bashrc ] ; then
  source ~/.bashrc
fi

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

#if [ -e ~/scripts/git-completion.bash ]; then
#    source ~/scripts/git-completion.bash
#fi
