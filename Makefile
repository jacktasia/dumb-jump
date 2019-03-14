CASK ?= cask
EMACS ?= emacs

all: test

test: unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

test-concurrent:
	@go run ert_runner.go -p ".*-ag-.*" -p ".*-rg-.*" test/dumb-jump-test.el

test-go:
	@go test -v

docker-build-test-runner:
	docker build . -t jacktasia/dumb-jump-test-runner:v3

docker-push-test-runner:
	docker push jacktasia/dumb-jump-test-runner:v3

test-all-local-in-docker:
	@bash run-all-in-docker-local.sh
