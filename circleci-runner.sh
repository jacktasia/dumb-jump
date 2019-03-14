#!/bin/bash

# source /home/travis/.bashrc
# source /home/travis/.bash_profile

PATH="/root/.evm/bin:$PATH"
export PATH="/root/.cask/bin:$PATH"
rg --version
ag --version
grep --version
emacs --version
git --version
pwd
make test
