
EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile dumb-jump.el

clean-elc:
	rm -f dumb-jump.elc

.PHONY:	all test unit compile
