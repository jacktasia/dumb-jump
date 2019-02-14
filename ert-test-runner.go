package main

import (
	"fmt"
	"io/ioutil"
	//	"os"
	"os/exec"
	"regexp"
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

	groups := []string{".*-git-grep-plus-ag.*", ".*-git-grep.*", ".*-rg-.*", ".*-ag-.*"}
	//groupCompiles := []*regexp.Regexp{}
	// TODO: then find all of the groups

	r, err := regexp.Compile("ert-deftest\\s([^\\s]+)\\s")

	matches := r.FindAllStringSubmatch(contents, -1)

	defs := []string{}
	for _, match := range matches {
		defs = append(defs, match[1])
	}

	// for _, ertTest := range defs {
	// 	fmt.Println("Running " + ertTest)
	// 	runErtTest(ertTest)
	// }
	//

	//fmt.Println(defs)
	runErtTestsConcurrently(defs)
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

func runErtTestsConcurrently(ertTests []string) {

	testCount := len(ertTests)

	jobs := make(chan string, testCount)
	results := make(chan string, testCount)

	// This starts up 3 workers, initially blocked
	// because there are no jobs yet.
	for w := 1; w <= 1; w++ {
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
	for a := 0; a <= testCount; a++ {
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
