#!/bin/bash
# Install sim links to version controlled cofig files

files=".tmate.conf .bash_logout .emacs .tmux.conf .bashrc .emacs.d .gitconfig .bash_profile .gitignore_global .git"
for file in $files; do
    ln -sFf $PWD/$file ~/
done
