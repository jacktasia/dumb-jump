#!/usr/bin/env bash
# test/run-in-container.sh
#
# Container entrypoint for running tests locally in Docker.
# Executed inside a Docker container built from test/Dockerfile.local,
# which is based on silex/emacs:<VERSION>-ci-cask.
#
# Emacs, Cask, git, and make are already in the base image.
# Cask dependencies are installed during docker build.
# This script verifies the toolchain and runs the tests.

set -euo pipefail

echo "========================================"
echo " dumb-jump local Docker test runner     "
echo " $(emacs --version | head -1)           "
echo "========================================"
echo ""

echo "==> Toolchain versions:"
echo -n "    emacs : "; emacs --version | head -1
echo -n "    rg    : "; rg --version | head -1
echo -n "    ag    : "; ag --version | head -1
echo -n "    git   : "; git --version
echo ""

echo ""
echo "==> Running ERT tests..."
cd /repo
# The built-in 'ert' reporter only fires after a test ends, making hangs
# invisible. We write a small shim file and load it via --load so a
# test-started hook prints each test name before it runs — a hang then
# shows exactly which test is stuck.
SHIM=$(mktemp /tmp/ert-runner-shim-XXXXXX.el)
cat > "$SHIM" <<'EOF'
(add-hook 'ert-runner-reporter-test-started-functions
          (lambda (stats test)
            (ert-runner-message "running: %S\n" (ert-test-name test))))
EOF
# stdbuf -oL: force line-buffered stdout so each line appears immediately
#   rather than being held in the C stdio buffer until exit.
# </dev/null: close Emacs's stdin. Without this, shell-command (used by ag
#   and git-grep tests) inherits the open stdin pipe and passes it to child
#   processes; ag detects the open pipe and waits on it instead of exiting.
stdbuf -oL cask exec ert-runner --verbose --load "$SHIM" </dev/null
rm -f "$SHIM"
