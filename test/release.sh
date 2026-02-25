#!/usr/bin/env bash
# test/release.sh
#
# Release workflow for dumb-jump:
# - validate release prerequisites
# - bump version when needed
# - tag the current commit
# - push branch + tag
# - create GitHub release
#
# Usage:
#   bash test/release.sh check
#   bash test/release.sh release
#   bash test/release.sh dry-run

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${REPO_ROOT}"

GH="${GH:-gh}"
RELEASE_REPO="${RELEASE_REPO:-jacktasia/dumb-jump}"
RELEASE_BRANCH="${RELEASE_BRANCH:-master}"
RELEASE_NOTES_FILE="${RELEASE_NOTES_FILE:-}"
RELEASE_GENERATE_NOTES="${RELEASE_GENERATE_NOTES:-1}"
RELEASE_VERSION="${RELEASE_VERSION:-}"
RELEASE_BUMP="${RELEASE_BUMP:-}"

usage() {
	echo "Usage: bash test/release.sh [check|release|dry-run]" >&2
}

require_cmd() {
	local cmd="$1"
	if ! command -v "${cmd}" >/dev/null 2>&1; then
		echo "error: ${cmd} is required." >&2
		exit 1
	fi
}

validate_semver() {
	local version="$1"
	if ! printf '%s' "${version}" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$'; then
		echo "error: version '${version}' is not semver (X.Y.Z)." >&2
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
	validate_semver "${version}"
	printf '%s' "${version}"
}

version_to_tag() {
	local version="$1"
	printf '%s' "${RELEASE_TAG:-v${version}}"
}

bump_version() {
	local version="$1"
	local bump="$2"
	local major minor patch
	IFS='.' read -r major minor patch <<<"${version}"
	case "${bump}" in
	patch)
		patch=$((patch + 1))
		;;
	minor)
		minor=$((minor + 1))
		patch=0
		;;
	major)
		major=$((major + 1))
		minor=0
		patch=0
		;;
	*)
		echo "error: RELEASE_BUMP must be one of: patch minor major." >&2
		exit 1
		;;
	esac
	printf '%s.%s.%s' "${major}" "${minor}" "${patch}"
}

tag_exists_locally() {
	local tag="$1"
	git rev-parse -q --verify "refs/tags/${tag}" >/dev/null
}

tag_exists_on_origin() {
	local tag="$1"
	git ls-remote --tags --exit-code origin "refs/tags/${tag}" >/dev/null 2>&1
}

tag_exists_anywhere() {
	local tag="$1"
	tag_exists_locally "${tag}" || tag_exists_on_origin "${tag}"
}

resolve_target_version() {
	local current_version current_tag
	current_version="$(read_version)"

	if [[ -n "${RELEASE_VERSION}" ]]; then
		validate_semver "${RELEASE_VERSION}"
		printf '%s' "${RELEASE_VERSION}"
		return
	fi

	if [[ -n "${RELEASE_BUMP}" ]]; then
		printf '%s' "$(bump_version "${current_version}" "${RELEASE_BUMP}")"
		return
	fi

	current_tag="$(version_to_tag "${current_version}")"
	if tag_exists_anywhere "${current_tag}"; then
		echo "info: ${current_tag} already exists; auto-bumping patch." >&2
		printf '%s' "$(bump_version "${current_version}" "patch")"
		return
	fi

	printf '%s' "${current_version}"
}

update_version_file() {
	local new_version="$1"
	local tmp_file
	tmp_file="$(mktemp "${TMPDIR:-/tmp}/dumb-jump-version-XXXXXX")"
	if ! awk -v version="${new_version}" '
	BEGIN { updated = 0 }
	{
		if (!updated && $0 ~ /^;; Version:[[:space:]]*/) {
			print ";; Version: " version
			updated = 1
		} else {
			print $0
		}
	}
	END { if (!updated) exit 2 }
	' dumb-jump.el >"${tmp_file}"; then
		rm -f "${tmp_file}"
		echo "error: failed to update dumb-jump.el version." >&2
		exit 1
	fi
	mv "${tmp_file}" dumb-jump.el
}

commit_version_bump() {
	local version="$1"
	git add dumb-jump.el
	git commit -m "bump version to ${version}" >/dev/null
	echo "Committed version bump: ${version}" >&2
}

prepare_version_for_release() {
	local current_version target_version
	current_version="$(read_version)"
	target_version="$(resolve_target_version)"

	if [[ "${target_version}" == "${current_version}" ]]; then
		echo "Release version: ${target_version}" >&2
		printf '%s' "${target_version}"
		return
	fi

	echo "Bumping version from ${current_version} to ${target_version}" >&2
	update_version_file "${target_version}"
	commit_version_bump "${target_version}"
	printf '%s' "${target_version}"
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
	if tag_exists_locally "${tag}"; then
		echo "error: local tag '${tag}' already exists." >&2
		exit 1
	fi
	if tag_exists_on_origin "${tag}"; then
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
	local current_version target_version tag
	require_cmd git
	require_cmd "${GH}"
	assert_on_release_branch
	assert_clean_tree

	current_version="$(read_version)"
	target_version="$(resolve_target_version)"
	tag="$(version_to_tag "${target_version}")"

	if [[ "${target_version}" != "${current_version}" ]]; then
		echo "Release check: would bump ${current_version} -> ${target_version}"
	fi

	assert_tag_does_not_exist "${tag}"
	assert_gh_authenticated
	echo "Release check passed for ${tag}"
}

release_create() {
	local target_version tag
	require_cmd git
	require_cmd "${GH}"
	assert_on_release_branch
	assert_clean_tree

	target_version="$(prepare_version_for_release)"
	tag="$(version_to_tag "${target_version}")"

	assert_tag_does_not_exist "${tag}"
	assert_gh_authenticated

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

release_dry_run() {
	local current_version target_version tag current_branch ready
	require_cmd git
	require_cmd "${GH}"

	current_version="$(read_version)"
	target_version="$(resolve_target_version)"
	tag="$(version_to_tag "${target_version}")"
	current_branch="$(git rev-parse --abbrev-ref HEAD)"
	ready=1

	echo "Release dry run:"
	echo "  current version: ${current_version}"
	echo "  target version : ${target_version}"
	echo "  target tag     : ${tag}"
	echo "  release branch : ${RELEASE_BRANCH}"
	echo "  release repo   : ${RELEASE_REPO}"
	if [[ -n "${RELEASE_NOTES_FILE}" ]]; then
		echo "  release notes  : file (${RELEASE_NOTES_FILE})"
	elif [[ "${RELEASE_GENERATE_NOTES}" == "1" ]]; then
		echo "  release notes  : generated"
	else
		echo "  release notes  : empty body"
	fi

	if [[ "${current_branch}" == "${RELEASE_BRANCH}" ]]; then
		echo "  branch check   : OK (${current_branch})"
	else
		echo "  branch check   : FAIL (current=${current_branch})"
		ready=0
	fi

	if [[ -n "$(git status --porcelain)" ]]; then
		echo "  tree check     : FAIL (working tree is dirty)"
		ready=0
	else
		echo "  tree check     : OK (clean)"
	fi

	if tag_exists_locally "${tag}"; then
		echo "  local tag      : FAIL (${tag} exists)"
		ready=0
	else
		echo "  local tag      : OK (missing)"
	fi

	if tag_exists_on_origin "${tag}"; then
		echo "  remote tag     : FAIL (${tag} exists on origin)"
		ready=0
	else
		echo "  remote tag     : OK (missing on origin)"
	fi

	if "${GH}" auth status >/dev/null 2>&1; then
		echo "  gh auth        : OK"
	else
		echo "  gh auth        : FAIL (not authenticated)"
		ready=0
	fi

	echo "  tests          : not run automatically"

	if [[ "${ready}" -eq 1 ]]; then
		echo "Result: ready to run release."
	else
		echo "Result: not ready."
		exit 1
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
	dry-run)
		release_dry_run
		;;
	*)
		usage
		exit 1
		;;
	esac
}

main "$@"
