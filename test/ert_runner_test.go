package main

import (
	"testing"
)

func TestExtractRunCount(t *testing.T) {
	s := "Ran 19 tests, 19 results as expected"
	result := extractRunCount(s)
	expected := 19
	if result != expected {
		t.Error("extractRunCount failed, got:", result, ", expected: ", expected)
	}
}

func TestParseTestNames(t *testing.T) {
	s := " passed   1/18  dumb-jump-get-git-grep-files-matching-symbol-test\npassed   2/18  dumb-jump-go-clojure-asterisk-test\npassed   3/18  dumb-jump-go-clojure-no-asterisk-test\npassed"
	result := parseTestNames(s)
	expected := []string{"dumb-jump-get-git-grep-files-matching-symbol-test", "dumb-jump-go-clojure-asterisk-test", "dumb-jump-go-clojure-no-asterisk-test"}
	if len(result) != len(expected) || result[0] != expected[0] || result[1] != expected[1] || result[2] != expected[2] {
		t.Error("parseTestNames failed, got:", result, ", expected: ", expected)
	}
}
