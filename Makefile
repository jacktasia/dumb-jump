CASK ?= cask
EMACS ?= emacs

all: test

test: unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

test-concurrent:
	@go run ert_runner.go test/dumb-jump-test.el

test-go:
	@go test -v
