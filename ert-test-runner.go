package main

import (
	"fmt"
	"io/ioutil"
	//	"os"
	"os/exec"
	"regexp"
	"strings"
	"time"
)

// TODO: this should run inside of a docker container with all the dependencies set!
func main() {

	fmt.Println("ert-test-runner!")

	dat, err := ioutil.ReadFile("/home/jack/code/dumb-jump/test/dumb-jump-test.el")
	if err != nil {
		panic(err)
	}

	contents := string(dat)

	//groupCompiles := []*regexp.Regexp{}
	// TODO: then find all of the groups
	groups := []string{".*-rg-.*", ".*-ag-.*"} // ".*-git-grep-plus-ag.*", ".*-git-grep.*",
	// "dumb-jump-shell-command-switch-.*", "dumb-jump-pick-grep-variant-.*", "dumb-jump-handle-results-.*", "dumb-jump-message-.*", "dumb-jump-generate-.*", "dumb-jump-go-.*", "dumb-jump-react-.*", "dumb-jump-generators-.*", "dumb-jump-get-lang-.*", "dumb-jump-test-grep-.*", "dumb-jump-run-.*", "dumb-jump-prompt-.*"}
	//groupCompiles := []*regexp.Regexp{}
	r, err := regexp.Compile("ert-deftest\\s([^\\s]+)\\s")

	matches := r.FindAllStringSubmatch(contents, -1)

	defs := []string{}
	for _, match := range matches {
		defs = append(defs, match[1])
	}

	// TODO:
	remainders := []string{}
	remainders = append(remainders, defs...)
	testGroups := [][]string{}
	var testGroup []string
	for _, group := range groups {
		//regexp.MatchString(group,
		remainders, testGroup = takeMatches(remainders, group)

		testGroups = append(testGroups, testGroup)
	}

	// for _, ertTest := range remainders {
	// 	//fmt.Println("Running " + ertTest)
	// 	runErtTest(ertTest)
	// }

	fmt.Println("defs len", len(defs))
	fmt.Println("remainders len", len(remainders))
	fmt.Println("test groups len", len(testGroups))

	fmt.Println("------------")
	fmt.Println(remainders)
	fmt.Println("Running....")

	//runErtTestsConcurrently(remainders)

	toRun := []string{}
	toRun = append(toRun, groups...)
	//toRun = append(toRun, remainders...)

	chunked := chunk(remainders, 30)

	for _, c := range chunked {
		toRun = append(toRun, makePattern(c))
	}

	//chunkSize := (len(logs) + numCPU - 1) / numCPU

	start := time.Now()
	runErtTestsConcurrently(toRun)
	end := time.Now()
	fmt.Println("------------")
	fmt.Println("Done. took", end.Sub(start))
}

func makePattern(items []string) string {
	return ("'\\(" + strings.Join(items, "\\|") + "\\)'")
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
		fmt.Println(j, "took", end.Sub(start))
		//result := "2"
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

func runErtTestsConcurrently(ertTests []string) {
	testCount := len(ertTests)

	jobs := make(chan string, testCount)
	results := make(chan string, testCount)

	workerCount := 6
	for w := 1; w <= workerCount; w++ {
		go worker(w, jobs, results)
	}

	// Here we send 5 `jobs` and then `close` that
	// channel to indicate that's all the work we have.
	for j := 0; j < testCount; j++ {
		jobs <- ertTests[j]
	}
	close(jobs)

	// Finally we collect all the results of the work.
	// for a := 1; a <= 5; a++ {
	fmt.Println("Waiting....")
	for a := 1; a <= testCount; a++ {
		fmt.Println("Done with")
		<-results
	}
}

func runErtTest(testName string) string {
	// cmdPrefix := "cask exec ert-runner -p "
	fmt.Println("Running " + testName)
	cmd := "cask"
	args := []string{"exec", "ert-runner", "-p", testName}
	out, err := exec.Command(cmd, args...).Output()

	if err != nil {
		fmt.Println("~~~~ ERRROR", testName, err, "--", string(out))

	}

	return string(out)
}
