package main

import (
	"fmt"
	"io/ioutil"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// TODO: this should run inside of a docker container with all the dependencies set!
// TODO: take file argument and list of `--serialize` and `--slow` patterns
// TODO: make helper that will download from this repo as raw and and run `go run ert-test-runner.go my-tests.el --slow '.*-ag-.*'
// TODO: add tests for the test runner

func maintest() {
	s := "Ran 19 tests, 19 results as expected"
	fmt.Println(extractRunCount(s))
}
func main() {

	fmt.Println("ert-test-runner!")

	dat, err := ioutil.ReadFile("/home/jack/code/dumb-jump/test/dumb-jump-test.el")
	if err != nil {
		panic(err)
	}

	contents := string(dat)

	//groupCompiles := []*regexp.Regexp{}
	// TODO: then find all of the groups
	//groups := []string{}
	//groups := []string{".*-rg-.*", ".*-ag-.*"}
	//standalones := []string{"dumb-jump-test-ag-rules-test"}
	standalones := []string{}

	groups := []string{".*-ag-.*", ".*-rg-.*"}
	r, err := regexp.Compile("ert-deftest\\s([^\\s]+)\\s")

	matches := r.FindAllStringSubmatch(contents, -1)

	defs := []string{}
	for _, match := range matches {
		defs = append(defs, match[1])
	}

	totalTests := len(defs)

	remainders := []string{}
	remainders = append(remainders, defs...)
	for _, standalone := range standalones {
		remainders, _ = takeMatches(remainders, standalone)
	}
	testGroups := [][]string{}
	var testGroup []string
	for _, group := range groups {
		remainders, testGroup = takeMatches(remainders, group)
		fmt.Println("Removed", testGroup)
		testGroups = append(testGroups, testGroup)
	}

	fmt.Println("defs len", len(defs))
	fmt.Println("remainders len", len(remainders))
	fmt.Println("test groups len", len(testGroups))

	fmt.Println("------------")
	//fmt.Println(remainders)
	fmt.Println("Running....")

	toRun := []string{}
	toRun = append(toRun, groups...)

	chunked := chunk(remainders, 30)
	for _, c := range chunked {
		toRun = append(toRun, makePattern(c))
	}

	// start running
	start := time.Now()
	totalRan := 0
	for _, standalone := range standalones {
		totalRan += extractRunCount(runErtTest(standalone))
	}

	totalRan += runErtTestsConcurrently(toRun)
	end := time.Now()
	fmt.Println("------------")
	fmt.Println("Done. Ran", totalRan, "tests of", totalTests, " and took", end.Sub(start))
}

func makePattern(items []string) string {
	return ("\\(" + strings.Join(items, "\\|") + "\\)")
}

func takeMatches(items []string, regex string) ([]string, []string) {
	r, err := regexp.Compile(regex)
	if err != nil {
		panic(err)
	}

	result := []string{}
	matches := []string{}
	for _, m := range items {
		if !r.MatchString(m) {
			result = append(result, m)
		} else {
			matches = append(matches, m)
		}
	}

	return result, matches
}

func worker(id int, jobs <-chan string, results chan<- string) {
	for j := range jobs {
		//fmt.Println("worker", id, "started  job", j)
		start := time.Now()
		result := runErtTest(string(j))
		end := time.Now()
		fmt.Println(j[0:5], "size", len(j), "took", end.Sub(start))
		// fmt.Println("worker", id, "finished job", j)
		results <- result
	}
}

func chunk(logs []string, chunkSize int) [][]string {
	var divided [][]string

	for i := 0; i < len(logs); i += chunkSize {
		end := i + chunkSize

		if end > len(logs) {
			end = len(logs)
		}

		divided = append(divided, logs[i:end])
	}

	return divided
}

func runErtTestsConcurrently(ertTests []string) int {
	testCount := len(ertTests)

	jobs := make(chan string, testCount)
	results := make(chan string, testCount)

	// TODO: base off cpu count?
	// spin up 6 workers
	workerCount := 6
	for w := 1; w <= workerCount; w++ {
		go worker(w, jobs, results)
	}

	// send the `jobs` and `close` that
	// channel to indicate that's all the work we have.
	for j := 0; j < testCount; j++ {
		jobs <- ertTests[j]
	}
	close(jobs)

	fmt.Println("Waiting....")

	// collect results
	// TODO: gather list of tests
	totalRan := 0
	for a := 1; a <= testCount; a++ {
		totalRan += extractRunCount(<-results)
	}

	return totalRan
}

// TODO: also extract the list of tests themselves so we can figure out exactly which ones were dropped
func extractRunCount(output string) int {
	r, err := regexp.Compile("Ran ([0-9]+) tests, ([0-9]+) results as expected")
	if err != nil {
		panic(err)
	}
	// "Ran 19 tests, 19 results as expected"

	rawMatches := r.FindAllStringSubmatch(output, -1)

	if len(rawMatches) != 1 {
		panic("no len match")
	}

	matches := rawMatches[0]
	fmt.Println(matches)
	if matches[1] != matches[2] {
		panic(matches[1] + "does not equal" + matches[2])
	}

	i, _ := strconv.Atoi(string(matches[1]))
	return i
}

func runErtTest(testName string) string {
	// cmdPrefix := "cask exec ert-runner -p "
	//fmt.Println("Running " + testName)
	cmd := "cask"
	args := []string{"exec", "ert-runner", "-p", testName}
	out, err := exec.Command(cmd, args...).Output()

	if err != nil {
		fmt.Println("~~~~ ERRROR", testName, err, "--", string(out))

	}

	return string(out)
}
