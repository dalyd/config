#!/bin/bash
# Install sim links to version controlled cofig files. For the mac

# Install general symlinks
./install_config.sh

# Link .bash_local
ln -sf $PWD/.bash_local_osx ~/.bash_local