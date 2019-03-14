#!/bin/bash

echo "Running dumb-jump tests in all supported versions of emacs via docker"

dj_run_name=dumb-jump-`date +%s`
dj_tmp_path=/tmp/$dj_run_name

cp -r . $dj_tmp_path
for emacs_version in emacs-24.3-travis emacs-24.4-travis emacs-24.5-travis emacs-25.1-travis emacs-25.2-travis emacs-26.1-travis
do
    dj_log_name="/tmp/${dj_run_name}-${emacs_version}.log"
    echo "Running $emacs_version..."
    echo "    Check $dj_log_name for output."

    docker run -e EVM_EMACS=$emacs_version -v $dj_tmp_path:/home/travis/dumb-jump -it jacktasia/dumb-jump-test-runner:v3 > $dj_log_name 2>&1

    if [[ $? != 0 ]]; then
       echo "$emacs_version tests failed!"
       echo "See $dj_log_name for more details"
       exit 1
    fi

    echo "    $emacs_version success!"
done
