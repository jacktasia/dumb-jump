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

# ----------------------------------------------------------------------------
# Rules
# -----

# Most recipes do not correspond to the presence of a file; they must be
# declared PHONY to avoid conflict with a file name.
#
.PHONY: all test unit install test-concurrent test-go setup \
        actions-test test-docker help


all: test

.cask:
	${CASK}

test: unit

unit: .cask
	${CASK} exec ert-runner

install:
	${CASK} install


test-concurrent: .cask
	@go run test/ert_runner.go -p ".*-ag-.*" -p ".*-rg-.*" test/dumb-jump-test.el

test-go:
	@go test ./... -v

setup:
	@bash test/github-actions-setup.sh

actions-test: install setup unit

# EMACS_VERSION can be overridden: make test-docker EMACS_VERSION=28.2
test-docker:
	@bash test/run-tests-locally.sh $(EMACS_VERSION)

# Include the test-some rule only when GNU Make is used.
#
# - GNU Make will try to include 'gnu.mk'.
# - NetBSD Make (bmake) will see '-include ' (empty) and do nothing as
#   expected because the content of gnu.mk can only be parsed by GNU Make.
# - The gnu.mk parses Make command line and builds the logic for the test-this
#   rule.
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
- make test-concurrent                        : execute all Ert tests, but concurrently.\n\
- make test-docker                            : run tests locally in Docker (default: Emacs 29.4)\n\
- make test-docker EMACS_VERSION=X.Y          : run tests in Docker with a specific Emacs version\n\
- make help                                   : prints this help.\n\n\
Notes:\n\
- 'test-concurrent' shows # of skipped tests due to unavailability of a search tool, others do not.\n\
- 'test-this' is only available when using GNU Make.\n\
- Use 'test-this' to identify a set of tests by complete or partial names.\n\
- 'test-docker' requires Docker. Supported Emacs versions: 26.3  27.2  28.2  29.4  30.2\n\n"

# ----------------------------------------------------------------------------
