#!/usr/bin/env bash
# test/run-in-container.sh
#
# Container entrypoint for running tests locally in Docker.
# Executed inside a Docker container built from test/Dockerfile.local,
# which is based on silex/emacs:<VERSION>-ci-cask.
#
# Emacs, Cask, git, and make are already in the base image.
# This script just verifies the toolchain and runs the tests.

set -euo pipefail

echo "========================================"
echo " dumb-jump local Docker test runner     "
echo " $(emacs --version | head -1)           "
echo "========================================"
echo ""

echo "==> Toolchain versions:"
echo -n "    emacs : "; emacs --version | head -1
echo -n "    cask  : "; cask --version
echo -n "    rg    : "; rg --version | head -1
echo -n "    ag    : "; ag --version | head -1
echo -n "    git   : "; git --version
echo ""

echo "==> Installing Emacs package dependencies (cask install)..."
cd /repo
make install

echo ""
echo "==> Running ERT tests (cask exec ert-runner)..."
make unit
