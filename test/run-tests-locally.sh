#!/usr/bin/env bash
# test/run-tests-locally.sh
#
# Run the test suite locally in a Docker container.
# Uses silex/emacs:<VERSION>-ci-cask as the base image (Emacs + Cask pre-installed).
# On Apple Silicon, Docker Desktop emulates amd64 via Rosetta 2 automatically.
#
# Usage:
#   bash test/run-tests-locally.sh [EMACS_VERSION]
#   make test-docker
#   make test-docker EMACS_VERSION=28.1
#
# Arguments:
#   EMACS_VERSION  Emacs version to test against (default: 29.4)
#                  Must be one of: 25.3 26.1 26.2 26.3 27.1 28.1 29.4 30.1
#
# Examples:
#   bash test/run-tests-locally.sh          # uses default (29.4)
#   bash test/run-tests-locally.sh 28.1
#   bash test/run-tests-locally.sh 25.3

set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────────────────

# Positional arg takes priority over env var; env var is the Make-friendly path.
EMACS_VERSION="${1:-${EMACS_VERSION:-29.4}}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DOCKERFILE="${REPO_ROOT}/test/Dockerfile.local"

# Tag images per Emacs version so different versions don't clobber each other's
# Docker layer cache.
IMAGE_TAG="dumb-jump-test:emacs-${EMACS_VERSION}"

# Emacs versions matching the CI matrix (from .github/workflows/test.yml)
SUPPORTED_VERSIONS="25.3 26.1 26.2 26.3 27.1 28.1 29.4 30.1"

# ── Validation ────────────────────────────────────────────────────────────────

if ! command -v docker &>/dev/null; then
    echo "ERROR: Docker is not installed or not on PATH." >&2
    echo "Install Docker Desktop: https://www.docker.com/products/docker-desktop/" >&2
    exit 1
fi

if ! docker info &>/dev/null 2>&1; then
    echo "ERROR: Docker daemon is not running. Please start Docker Desktop." >&2
    exit 1
fi

version_ok=0
for v in $SUPPORTED_VERSIONS; do
    if [[ "$v" == "$EMACS_VERSION" ]]; then
        version_ok=1
        break
    fi
done
if [[ $version_ok -eq 0 ]]; then
    echo "ERROR: Emacs version '${EMACS_VERSION}' is not a supported version." >&2
    echo "Supported versions: ${SUPPORTED_VERSIONS}" >&2
    exit 1
fi

# ── Info ──────────────────────────────────────────────────────────────────────

echo "==> Running tests locally in Docker"
echo "    Emacs version : ${EMACS_VERSION}"
echo "    Docker image  : ${IMAGE_TAG}"
echo "    Repo root     : ${REPO_ROOT}"
echo ""

# ── Build image ───────────────────────────────────────────────────────────────
# Pass EMACS_VERSION as a build arg so the Dockerfile pulls the matching
# silex/emacs base image. Build context is repo root so the Dockerfile can
# COPY test/run-in-container.sh.
#
# On Apple Silicon, the silex/emacs images are amd64-only; Docker Desktop
# handles the emulation via Rosetta 2 (--platform linux/amd64 is set in
# the Dockerfile's FROM line).
#
# Docker layer caching means subsequent builds for the same Emacs version are
# fast. Source changes invalidate the repo snapshot layer only.

echo "==> Building image for Emacs ${EMACS_VERSION}..."
echo "    (First pull of silex/emacs:${EMACS_VERSION}-ci-cask takes ~1-2 min;"
echo "     subsequent runs use Docker's layer cache)"
docker build \
    --file      "${DOCKERFILE}" \
    --tag       "${IMAGE_TAG}" \
    --build-arg "EMACS_VERSION=${EMACS_VERSION}" \
    "${REPO_ROOT}"

echo ""

# ── Run tests ─────────────────────────────────────────────────────────────────
# The image already contains a snapshot of the repo from build time.
# --rm cleans up the container after it exits.

echo "==> Launching test container..."
# Note: no --tty here. Emacs runs in batch/noninteractive mode and spawns
# subprocesses (ag, rg, grep) via shell-command. With --tty, those child
# processes inherit the pseudo-TTY and can block waiting for terminal input
# instead of exiting when done. Plain pipe I/O (no --tty) keeps them clean.
docker run \
    --rm \
    "${IMAGE_TAG}"
