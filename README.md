# dumb-jump

[![Join the chat at https://gitter.im/jacktasia/dumb-jump](https://badges.gitter.im/jacktasia/dumb-jump.svg)](https://gitter.im/jacktasia/dumb-jump?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/jacktasia/dumb-jump.svg)](https://travis-ci.org/jacktasia/dumb-jump) [![Coverage Status](https://coveralls.io/repos/jacktasia/dumb-jump/badge.svg?branch=master&service=github)](https://coveralls.io/github/jacktasia/dumb-jump?branch=master)

## About
**Dumb Jump** is an Emacs "jump to definition" package with support for multiple programming languages that favors "just working" over speed or accuracy. This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (TAGS) or persistent background processes.

Dumb Jump uses `grep` <sup id="a1">[1](#f1)</sup> to find potential definitions of a variable/function (under point) via a set of regular expressions based on the file extension of the current buffer. Once there is a list of potential candidates they are run through a set of heuristic methods to find the best candidate to jump to. Right now these methods are general and shared among languages but in the future will almost certainly have to be customized to increase accuracy and support more languages.

It *seems* like in most situations for JavaScript, Emacs Lisp, Python, and go it does a good job of finding what you want. It can be slow if a project is large, but it can usually be sped up by creating a `.dumbjump` file in your project's root directory with paths that should be excluded (see configuration).

<a name="f1"></a>
**1.** I could see support for `ack` or `ag` support instead, but `grep` is pretty much everywhere, so I felt it made sense to go first.


## Why?

I wanted "jump to definition" functionality to "just work" in emacs. I use IntelliJ for Java and it's basically the only thing I miss when I switch back to emacs for work in any other language. There are certainly other packages that offer this type of functionality, and honestly, many are faster and have better accuracy, but they all require significantly more configuration and are often limited to a particular language. These may be worth setting up if you are in a specific in project or language often.

Here is a list of potential alternatives packages for emacs:

* [Tags](http://www.gnu.org/software/emacs/manual/html_node/emacs/Tags.html) supports multiple languages
* [GNU Global](http://www.gnu.org/software/global/) supports multiple languages
* [Tern](http://ternjs.net/) for JavaScript
* [elpy](https://github.com/jorgenschaefer/elpy) for Python
* [rob](https://github.com/dgutov/robe) for Ruby

Most of these were sourced from this [emacs StackExchange answer](http://emacs.stackexchange.com/questions/10125/can-emacs-support-go-to-declaration-of-function-in-an-entire-project)

## Usage

The recommended way to install Dumb Jump *will be* via package.el

## Configuration

### Basic
There are two interactive Dumb Jump functions:

* `dumb-jump-go` core functionality. Attempts to jump to the definition for the thing under point
* `dumb-jump-back` jumps back to where you were when you jumped. These are chained so if you go down a rabbit how you can get back out or where you want to be.

## Customize

* `(setq dumb-jump-quiet t)` in your `.emacs` if Dumb Jump is too chatty.
* To support more languages and/or definition types `add-to-list` on `dumb-jump-find-rules` (see source code).

## Contributing

Feedback is very welcome via GitHub issues. I will positively consider supporting other languages either via issue request or PR. If submitting a PR then please add tests as well.

## Running Tests
Requires [Cask](https://github.com/cask/cask).

    cd /path/to/dumb-jump
    cask
    make test
