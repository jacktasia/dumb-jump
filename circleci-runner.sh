#!/bin/bash

source /root/.bashrc

evm install $EVM_EMACS --use --skip # should be a noop when cache

PATH="/root/.evm/bin:$PATH"
export PATH="/root/.cask/bin:$PATH"
rg --version
ag --version
grep --version
emacs --version
git --version
pwd
cd /home/travis/dumb-jump
cask install
make test
