package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"
)

var (
	RegexParsePassedName = regexp.MustCompile("passed\\s+[0-9]+/[0-9]+\\s+(.+)\\b")
)

// TODO: this should run inside of a docker container with all the dependencies set!
// TODO: take file argument and list of `--serialize` and `--slow` patterns
// TODO: make helper that will download from this repo as raw and and run `go run ert-test-runner.go my-tests.el --slow '.*-ag-.*'

func main() {
	fmt.Println("ert-test-runner!")
	fmt.Println("----------------")
	if len(os.Args) < 2 {
		fmt.Println("Expects one postional argument of ert test file!")
		os.Exit(1)
	}

	//dat, err := ioutil.ReadFile("/home/jack/code/dumb-jump/test/dumb-jump-test.el")
	fmt.Println("Parsing", os.Args[1], "for tests...")
	dat, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}

	contents := string(dat)

	//standalones := []string{"dumb-jump-test-ag-rules-test"}
	standalones := []string{}
	groups := []string{".*-ag-.*", ".*-rg-.*"}

	r, err := regexp.Compile("[ ;]*\\(ert-deftest\\s([^\\s]+)\\s")
	matches := r.FindAllStringSubmatch(contents, -1)

	defs := []string{}
	for _, match := range matches {
		wholeMatch := strings.TrimSpace(match[0])
		testName := strings.TrimSpace(match[1])
		if wholeMatch[0:1] != ";" {
			defs = append(defs, testName)
		}
	}
	totalTests := len(defs)

	// split out passsed `standalones` and `groups` from `remainders` (which are chunked)
	remainders := []string{}
	remainders = append(remainders, defs...)
	for _, standalone := range standalones {
		remainders, _ = takeMatches(remainders, standalone)
	}
	testGroups := [][]string{}
	var testGroup []string
	for _, group := range groups {
		remainders, testGroup = takeMatches(remainders, group)
		//fmt.Println("Removed from remainders based on X: ", testGroup)
		testGroups = append(testGroups, testGroup)
	}

	fmt.Println("------------")
	// go through and chunk the remainders into groups
	toRun := []string{}
	toRun = append(toRun, groups...)
	chunked := chunk(remainders, 30)
	for _, c := range chunked {
		toRun = append(toRun, makePattern(c))
	}

	// start running
	start := time.Now()
	ranTests := []string{}
	for _, standalone := range standalones {
		ranTests = append(ranTests, parseTestNames(runErtTest(standalone))...)
	}

	ranTests = append(ranTests, runErtTestsConcurrently(toRun)...)
	missing := findMissing(ranTests, defs)
	end := time.Now()

	// DONE, report
	fmt.Println("------------")
	totalRanTests := totalTests - len(missing)
	if len(missing) > 0 {
		fmt.Println("Did not run!", missing)
	}
	fmt.Println("Done. Ran", totalRanTests, "tests of", totalTests, " and took", end.Sub(start))
}

func findMissing(ranTests []string, defs []string) []string {
	missing := []string{}
	for _, testName := range defs {
		if !contains(ranTests, testName) {
			missing = append(missing, testName)
		}
	}
	return missing
}

func contains(s []string, e string) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}

func makePattern(items []string) string {
	return strings.Replace(("\\(" + strings.Join(items, "\\|") + "\\)"), "?", ".", -1)
}

func takeMatches(items []string, regex string) ([]string, []string) {
	r, err := regexp.Compile(regex)
	if err != nil {
		// TODO: friendly error message that input is bad
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
		ranTests := parseTestNames(result)
		fmt.Println("Worker", id, "ran", len(ranTests), "tests in", end.Sub(start))
		//fmt.Println("id", id, "chunk finished", "size", len(j), "took", end.Sub(start))
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

func runErtTestsConcurrently(ertTests []string) []string {
	testCount := len(ertTests)

	jobs := make(chan string, testCount)
	results := make(chan string, testCount)

	// TODO: base off cpu count?
	// spin up workers
	workerCount := 4
	for w := 1; w <= workerCount; w++ {
		go worker(w, jobs, results)
	}

	// send the `jobs` and `close` that
	// channel to indicate that's all the work we have.
	for j := 0; j < testCount; j++ {
		jobs <- ertTests[j]
	}
	close(jobs)

	fmt.Println("Waiting for jobs to complete....")

	// collect results
	ranTests := []string{}
	for a := 1; a <= testCount; a++ {
		ranTests = append(ranTests, parseTestNames(<-results)...)
	}

	return ranTests
}

// TODO: unused
func extractRunCount(output string) int {
	r := regexp.MustCompile("Ran ([0-9]+) tests, ([0-9]+) results as expected")
	rawMatches := r.FindAllStringSubmatch(output, -1)

	if len(rawMatches) != 1 {
		return -1
	}

	matches := rawMatches[0]
	if matches[1] != matches[2] {
		return -1
	}

	i, _ := strconv.Atoi(string(matches[1]))
	return i
}

func parseTestNames(output string) []string {
	rawMatches := RegexParsePassedName.FindAllStringSubmatch(output, -1)

	results := []string{}
	for _, rm := range rawMatches {
		results = append(results, strings.Replace(rm[1], "\\?", "?", -1))
	}

	return results
}

func runErtTest(testName string) string {
	// cmdPrefix := "cask exec ert-runner -p "
	// fmt.Println("Running " + testName)
	// TODO: remove dependency on cask/ert-runner
	cmd := "cask"
	args := []string{"exec", "ert-runner", "-p", testName}
	out, err := exec.Command(cmd, args...).Output()

	if err != nil {
		fmt.Println("~~~~ ERRROR", testName, err, "--", string(out))
	}

	return string(out)
}
