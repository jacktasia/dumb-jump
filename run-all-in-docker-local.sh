#!/bin/bash

for emacs_version in emacs-24.3-travis emacs-24.4-travis emacs-24.5-travis emacs-25.1-travis emacs-25.2-travis emacs-26.1-travis
do
    echo "Running $emacs_version..."
    docker run -e EVM_EMACS=$emacs_version -v $(pwd):/home/travis/dumb-jump -it jacktasia/dumb-jump-test-runner:v3 || exit 1
done

rm -rf .cask/
