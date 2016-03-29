# dumb-jump

[![Join the chat at https://gitter.im/jacktasia/dumb-jump](https://badges.gitter.im/jacktasia/dumb-jump.svg)](https://gitter.im/jacktasia/dumb-jump?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/jacktasia/dumb-jump.svg)](https://travis-ci.org/jacktasia/dumb-jump) [![Coverage Status](https://coveralls.io/repos/jacktasia/dumb-jump/badge.svg?branch=master&service=github)](https://coveralls.io/github/jacktasia/dumb-jump?branch=master) [![MELPA](http://melpa.org/packages/dumb-jump-badge.svg)](http://melpa.org/#/dumb-jump) [![MELPA Stable](https://stable.melpa.org/packages/dumb-jump-badge.svg)](https://stable.melpa.org/#/dumb-jump)

## About
**Dumb Jump** is an Emacs "jump to definition" package with support for multiple programming languages that favors "just working". This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (TAGS) or persistent background processes.


#### How  it works
Dumb Jump uses [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) `ag` (and falls back to `grep`) to find potential definitions of a function or variable under point. It uses a set of regular expressions based on the file extension, or `major-mode`, of the current buffer. Currently the `ack` matches are run through a shared set of heuristic methods to find the best candidate to jump to. If it can't decide it will present the user with a list in a pop-menu.

#### Success Rate
For the currently [supported languages](#supported-languages) it seems to do a good job of finding what you want. If you find a case where it does not work as expected do not hesitate to [open an issue](https://github.com/jacktasia/dumb-jump/issues). It can be slow if it needs to use `grep` and/or a project is large. Although it can be sped up by [installing `ag`](https://github.com/ggreer/the_silver_searcher#installing) and/or creating a `.dumbjump` file in your project's root directory with paths that should be excluded ([see configuration](#configuration)).


## Supported Languages

There is currently basic support for the following languages:

* JavaScript
* Emacs Lisp
* Python
* Go
* PHP
* Ruby
* Clojure
* Faust

If you have any issues with the existing languages, or you want support for another one, then please [open an issue](https://github.com/jacktasia/dumb-jump/issues). PRs are also welcome.

## Why?

I wanted "jump to definition" functionality to "just work" in emacs. I use IntelliJ for Java and this functionality is basically the only thing I miss when I switch back to emacs for work in other languages. There are certainly other packages that offer this type of functionality, but they all require significantly more configuration and are often limited to a particular language. An alternative may be worth setting up if you are in a specific project or language often (see [alternatives](#alternatives)).

## Installing

The recommended way to install Dumb Jump is via `package.el`. It's available on [MELPA](http://melpa.org/#/dumb-jump): <kbd>M-x</kbd> `package-install dumb-jump`


#### Installing `ag`

Dumb Jump performs best with The Silver Searcher `ag` installed on your system. Please see their [install instructions](https://github.com/ggreer/the_silver_searcher#installing)


## Usage

#### Basic

Adding `(dumb-jump-mode)` to your `.emacs` will enable the keybindings for two interactive Dumb Jump functions:

* `dumb-jump-go` <kbd>C-M g</kbd> core functionality. Attempts to jump to the definition for the thing under point
* `dumb-jump-back` <kbd>C-M p</kbd> jumps back to where you were when you jumped. These are chained so if you go down a rabbit hole you can get back out or where you want to be.
* `dumb-jump-quick-look` <kbd>C-M q</kbd> like `dumb-jump-go` but shows tooltip with `file`, `line`, and `context`

## Configuration

##### Excluding project directories

Dumb Jump will automatically look for a project root. If it's not finding one then either put a `.dumbjump` file in your project root and optionally add excluded directories to make it faster.

##### Example `.dumbjump`

    -tests
    -node_modules
    -build
    -images
    +../some-lib/src
    +/usr/lib/src

##### `.emacs` options

* `(setq dumb-jump-default-project "~/code")` to change default project if one is not found (defaults to `~`)
* `(setq dumb-jump-quiet t)` if Dumb Jump is too chatty.
*
* To support more languages and/or definition types use `add-to-list` on `dumb-jump-find-rules` (see source code).

## Contributing

Feedback is very welcome via GitHub issues. I will positively consider supporting other languages either via issue request or PR. If submitting a PR then please add tests as well.

## Running Tests
Requires [Cask](https://github.com/cask/cask).

    cd /path/to/dumb-jump
    cask
    make test

## Alternatives

Here is a list of potential alternatives packages for emacs:

* [Tags](http://www.gnu.org/software/emacs/manual/html_node/emacs/Tags.html) supports multiple languages
* [GNU Global](http://www.gnu.org/software/global/) supports multiple languages
* [Tern](http://ternjs.net/) for JavaScript
* [elpy](https://github.com/jorgenschaefer/elpy) for Python
* [robe](https://github.com/dgutov/robe) for Ruby

Most of these were sourced from this [emacs StackExchange answer](http://emacs.stackexchange.com/questions/10125/can-emacs-support-go-to-declaration-of-function-in-an-entire-project)
