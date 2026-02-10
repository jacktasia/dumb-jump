# MAKEFILE FILE: gnu.mk
#
# Purpose   : GNU Make compatible logic, conditionally included in Makefile.
# ----------------------------------------------------------------------------

# Special rule for: 'make test-this'
# ----------------------------------
#
# When the first target is test-this, the following arguments on make command
# line are interpreted as names of ERT test functions and make then issues a
# cask exec ert-runner for those tests only.
#
# The following logic builds a properly formatted string in the TEST_NAMES
# variable by first building a space-separated list of test names in the
# TEST_NAMES_LIST variables and then replaces the spaces by the \| separator.
# The special empty and space variables are first required to create a
# variable that holds one space character. There must NOT be any trailing
# spaces inside this file (and that should always be the case in Make files)!

# ----------------------------------------------------------------------------
# Code
# ----
#

CASK ?= cask

empty :=
space := $(empty) $(empty)

# If the first argument is "test-this" then execute the specified test(s).
ifeq (test-this,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test-this"
  TEST_NAMES_LIST := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  TEST_NAMES := $(subst $(space),\|,$(TEST_NAMES_LIST))
  # ...and turn them into do-nothing targets when there are some.
ifneq ($(strip $(TEST_NAMES_LIST)),)
   $(eval $(TEST_NAMES_LIST):;@:)
endif
endif


.PHONY: test-this

test-this: .cask
ifeq ($(strip $(TEST_NAMES)),)
	@printf -- "ERROR: No test names provided after test-this. Aborting.\n"
	@exit 1
else
	${CASK} exec ert-runner -p "$(TEST_NAMES)"
endif

# ----------------------------------------------------------------------------
