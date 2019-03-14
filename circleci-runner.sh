#!/bin/bash

source /root/.bashrc
source /root/.bash_profile

PATH="/root/.evm/bin:$PATH"
export PATH="/root/.cask/bin:$PATH"
rg --version
ag --version
grep --version
emacs --version
git --version
pwd
make test
