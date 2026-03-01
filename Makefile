# Build Control Makefile
#
# - Best used with GNU Make.
# - Support NetBSD bmake (will issue a warning on the -include line; ignore it)
# - If support for smake (Jörg Schilling make) is needed, a SMakefile should be written.

# Identify tools:

# Set this to detect if GNU Make is being used.
_GNU_MAKE_ONLY_FILE = $(if $(MAKE_VERSION),gnu.mk)

# Set these but don't override values from the environment if they are set.
CASK ?= cask
EMACS ?= emacs
EMACS_VERSION ?= 29.4
GH ?= gh
RELEASE_REPO ?= jacktasia/dumb-jump
RELEASE_BRANCH ?= master
RELEASE_VERSION ?=
RELEASE_BUMP ?=
RELEASE_NOTES_FILE ?=
RELEASE_GENERATE_NOTES ?= 1

# ----------------------------------------------------------------------------
# Rules
# -----

# Most recipes do not correspond to the presence of a file; they must be
# declared PHONY to avoid conflict with a file name.
#
.PHONY: all test unit install setup \
        actions-test test-docker test-this-docker \
        release-dry-run release-check release help


all: test

.cask:
	${CASK}

test: unit

unit: .cask
	@rm -f dumb-jump.elc
	${CASK} exec ert-runner

install:
	${CASK} install

setup:
	@bash test/github-actions-setup.sh

actions-test: install setup unit

# EMACS_VERSION can be overridden: make test-docker EMACS_VERSION=28.2
test-docker:
	@bash test/run-tests-locally.sh $(EMACS_VERSION)

release-dry-run:
	@GH="$(GH)" \
	 RELEASE_REPO="$(RELEASE_REPO)" \
	 RELEASE_BRANCH="$(RELEASE_BRANCH)" \
	 RELEASE_VERSION="$(RELEASE_VERSION)" \
	 RELEASE_BUMP="$(RELEASE_BUMP)" \
	 RELEASE_TAG="$(RELEASE_TAG)" \
	 RELEASE_NOTES_FILE="$(RELEASE_NOTES_FILE)" \
	 RELEASE_GENERATE_NOTES="$(RELEASE_GENERATE_NOTES)" \
	 bash test/release.sh dry-run

release-check:
	@GH="$(GH)" \
	 RELEASE_REPO="$(RELEASE_REPO)" \
	 RELEASE_BRANCH="$(RELEASE_BRANCH)" \
	 RELEASE_VERSION="$(RELEASE_VERSION)" \
	 RELEASE_BUMP="$(RELEASE_BUMP)" \
	 RELEASE_TAG="$(RELEASE_TAG)" \
	 RELEASE_NOTES_FILE="$(RELEASE_NOTES_FILE)" \
	 RELEASE_GENERATE_NOTES="$(RELEASE_GENERATE_NOTES)" \
	 bash test/release.sh check

release:
	@GH="$(GH)" \
	 RELEASE_REPO="$(RELEASE_REPO)" \
	 RELEASE_BRANCH="$(RELEASE_BRANCH)" \
	 RELEASE_VERSION="$(RELEASE_VERSION)" \
	 RELEASE_BUMP="$(RELEASE_BUMP)" \
	 RELEASE_TAG="$(RELEASE_TAG)" \
	 RELEASE_NOTES_FILE="$(RELEASE_NOTES_FILE)" \
	 RELEASE_GENERATE_NOTES="$(RELEASE_GENERATE_NOTES)" \
	 bash test/release.sh release

# Include the test-some rule only when GNU Make is used.
#
# - GNU Make will try to include 'gnu.mk'.
# - NetBSD Make (bmake) will see '-include ' (empty) and do nothing as
#   expected because the content of gnu.mk can only be parsed by GNU Make.
# - The gnu.mk parses Make command line and builds the logic for the test-this
#   and test-this-docker rules.
#
-include $(_GNU_MAKE_ONLY_FILE)


help:
	@printf -- "\n\
Execute dumb-jump Ert tests.\n\
The following targets are supported:\n\
\n\
- make                                        : same as 'make test'\n\
- make install                                : install all dependencies specified in Cask file\n\
- make unit                                   : execute all Ert tests\n\
- make test                                   : execute all Ert tests\n\
- make test-this T1 [T2...]                   : execute specified Ert test(s) T1, T2...\n\
- make test-this-docker T1 [T2...]            : execute specified Ert test(s) in Docker\n\
- make test-concurrent                        : execute all Ert tests, but concurrently.\n\
- make test-docker                            : run tests locally in Docker (default: Emacs 29.4)\n\
- make test-docker EMACS_VERSION=X.Y          : run tests in Docker with a specific Emacs version\n\
- make release-dry-run                        : show planned release version/tag and readiness checks\n\
- make release-check                          : validate release prerequisites\n\
- make release                                : tag/push current version and create GitHub release\n\
- make help                                   : prints this help.\n\n\
Notes:\n\
- 'test-concurrent' shows # of skipped tests due to unavailability of a search tool, others do not.\n\
- 'test-this' and 'test-this-docker' are only available when using GNU Make.\n\
- Use 'test-this' to identify a set of tests by complete or partial names.\n\
- Use 'test-this-docker' to run a filtered test set in Docker.\n\
- 'test-docker' requires Docker. Supported Emacs versions: 26.1 26.3 27.2 28.2 29.4 30.1 30.2\n\
- 'release' does not run tests; run them manually before cutting a release.\n\
- 'release' expects a clean '$(RELEASE_BRANCH)' checkout with ';; Version:' set in dumb-jump.el.\n\
- Default release bump behavior: if current vX.Y.Z tag exists, patch bumps to next version.\n\
- Optional release vars: RELEASE_VERSION=X.Y.Z RELEASE_BUMP=patch|minor|major RELEASE_TAG=vX.Y.Z RELEASE_NOTES_FILE=path RELEASE_GENERATE_NOTES=0\n\n"

# ----------------------------------------------------------------------------
