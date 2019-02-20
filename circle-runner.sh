#!/bin/bash


# sudo chmod 777 -R .
# echo "installing $EVM_EMACS"
# #evm uninstall emacs-24.3-travis
# evm install $EVM_EMACS --use --skip
# emacs --version
# ls

# cask
#
# make test-concurrent

source /home/travis/.bashrc
source /home/travis/.bash_profile
pwd
make test
