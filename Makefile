CASK ?= cask
EMACS ?= emacs

all: test

test: unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

test-concurrent:
	cask
	@go run test/ert_runner.go -p ".*-ag-.*" -p ".*-rg-.*" test/dumb-jump-test.el

test-go:
	@go test ./... -v

docker-build-test-runner:
	docker build . -t jacktasia/dumb-jump-test-runner:v3 -f test/Dockerfile

docker-push-test-runner:
	docker push jacktasia/dumb-jump-test-runner:v3

test-all-in-docker:
	@bash test/run-local-in-docker.sh all

test-in-docker:
	@bash test/run-local-in-docker.sh current

setup:
	@bash test/github-actions-setup.sh

actions-test: install setup unit
