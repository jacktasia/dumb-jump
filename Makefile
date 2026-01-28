CASK ?= cask
EMACS ?= emacs
ERT_EVAL := (setq ert-batch-print-level 10 ert-batch-print-length 120)

all: test

test: unit

unit:
	${CASK} exec ert-runner 

unit-debug:
	${CASK} --version && ${CASK} list
	timeout 1m ${CASK} exec emacs -Q --batch --eval "(message \"emacs started\")"
	timeout 5m ${CASK} exec emacs -Q --batch -l ert -l test/test-helper.el -l test/dumb-jump-test.el -f ert-run-tests-batch-and-exit
	${CASK} exec ert-runner -l test/ci-ert-settings.el

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

actions-test: install setup unit-debug
