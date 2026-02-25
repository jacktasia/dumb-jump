#!/usr/bin/env bash
# test/release.sh
#
# Release workflow for dumb-jump:
# - validate release prerequisites
# - run tests
# - tag the current commit
# - push branch + tag
# - create GitHub release
#
# Usage:
#   bash test/release.sh check
#   bash test/release.sh release

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${REPO_ROOT}"

GH="${GH:-gh}"
RELEASE_REPO="${RELEASE_REPO:-jacktasia/dumb-jump}"
RELEASE_BRANCH="${RELEASE_BRANCH:-master}"
RELEASE_NOTES_FILE="${RELEASE_NOTES_FILE:-}"
RELEASE_GENERATE_NOTES="${RELEASE_GENERATE_NOTES:-1}"

usage() {
	echo "Usage: bash test/release.sh [check|release]" >&2
}

require_cmd() {
	local cmd="$1"
	if ! command -v "${cmd}" >/dev/null 2>&1; then
		echo "error: ${cmd} is required." >&2
		exit 1
	fi
}

read_version() {
	local version
	version="$(sed -n 's/^;; Version:[[:space:]]*//p' dumb-jump.el | head -n 1)"
	if [[ -z "${version}" ]]; then
		echo "error: could not read version from dumb-jump.el." >&2
		exit 1
	fi
	if ! printf '%s' "${version}" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$'; then
		echo "error: version '${version}' is not semver (X.Y.Z)." >&2
		exit 1
	fi
	printf '%s' "${version}"
}

read_tag() {
	local version
	version="$(read_version)"
	printf '%s' "${RELEASE_TAG:-v${version}}"
}

assert_on_release_branch() {
	local current_branch
	current_branch="$(git rev-parse --abbrev-ref HEAD)"
	if [[ "${current_branch}" != "${RELEASE_BRANCH}" ]]; then
		echo "error: checkout '${RELEASE_BRANCH}' before releasing." >&2
		exit 1
	fi
}

assert_clean_tree() {
	if [[ -n "$(git status --porcelain)" ]]; then
		echo "error: working tree is not clean." >&2
		exit 1
	fi
}

assert_tag_does_not_exist() {
	local tag="$1"
	if git rev-parse -q --verify "refs/tags/${tag}" >/dev/null; then
		echo "error: local tag '${tag}' already exists." >&2
		exit 1
	fi
	if git ls-remote --tags --exit-code origin "refs/tags/${tag}" >/dev/null 2>&1; then
		echo "error: remote tag '${tag}' already exists on origin." >&2
		exit 1
	fi
}

assert_gh_authenticated() {
	if ! "${GH}" auth status >/dev/null 2>&1; then
		echo "error: not authenticated with GitHub CLI (${GH})." >&2
		exit 1
	fi
}

release_check() {
	local tag
	require_cmd git
	require_cmd "${GH}"
	tag="$(read_tag)"
	assert_on_release_branch
	assert_clean_tree
	assert_tag_does_not_exist "${tag}"
	assert_gh_authenticated
	echo "Running tests before release..."
	make --no-print-directory test
}

release_create() {
	local tag
	tag="$(read_tag)"
	release_check
	echo "Tagging ${tag} at $(git rev-parse --short HEAD)"
	git tag "${tag}"
	echo "Pushing ${RELEASE_BRANCH} and ${tag} to origin"
	git push origin "HEAD:refs/heads/${RELEASE_BRANCH}"
	git push origin "${tag}"
	echo "Creating GitHub release ${tag}"
	if [[ -n "${RELEASE_NOTES_FILE}" ]]; then
		"${GH}" release create "${tag}" \
			--repo "${RELEASE_REPO}" \
			--target "${RELEASE_BRANCH}" \
			--title "${tag}" \
			--notes-file "${RELEASE_NOTES_FILE}"
	elif [[ "${RELEASE_GENERATE_NOTES}" == "1" ]]; then
		"${GH}" release create "${tag}" \
			--repo "${RELEASE_REPO}" \
			--target "${RELEASE_BRANCH}" \
			--title "${tag}" \
			--generate-notes
	else
		"${GH}" release create "${tag}" \
			--repo "${RELEASE_REPO}" \
			--target "${RELEASE_BRANCH}" \
			--title "${tag}"
	fi
}

main() {
	if [[ $# -ne 1 ]]; then
		usage
		exit 1
	fi
	case "$1" in
	check)
		release_check
		;;
	release)
		release_create
		;;
	*)
		usage
		exit 1
		;;
	esac
}

main "$@"
