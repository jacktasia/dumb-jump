#!/bin/bash

source /home/travis/.bashrc
source /home/travis/.bash_profile
sudo chmod 777 -R .
echo "installing $EVM_EMACS"
#evm uninstall emacs-24.3-travis
evm install $EVM_EMACS --use --skip
emacs --version
ls

# cask
# make test
# make test-concurrent
