# dumber-jump

![CI](https://github.com/zenspider/dumber-jump/workflows/CI/badge.svg) [![Coverage Status](https://coveralls.io/repos/zenspider/dumber-jump/badge.svg?branch=master&service=github&x=1)](https://coveralls.io/github/zenspider/dumber-jump?branch=master) [![MELPA](http://melpa.org/packages/dumber-jump-badge.svg?x=6)](http://melpa.org/#/dumber-jump) [![MELPA Stable](https://stable.melpa.org/packages/dumber-jump-badge.svg?x=1)](https://stable.melpa.org/#/dumber-jump)

![Dumber Jump GIF](media/dumb-jump-example-v2.gif?raw=true)

## About
**Dumber Jump** is an Emacs "jump to definition" package with support for 50+ programming languages that favors "just working". This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (TAGS) or persistent background processes. Dumber Jump requires at least GNU Emacs `28.1`.

**Dumber Jump** is a fork from [dumb-jump] that removes as many configuration options as possible to go back to the roots of being a fairly config-free "dumb" jump. This version doesn't support other searchers, doesn't support alternative display methods, and only interfaces through xref.

#### How  it works
Dumber Jump uses [ripgrep](https://github.com/BurntSushi/ripgrep) `rg` to find potential definitions of a function or variable under point. It uses a set of regular expressions based on the file extension, or `major-mode`, of the current buffer. The matches are run through a shared set of heuristic methods to find the best candidate to jump to. If it can't decide it will present the user with a list.

#### Success Rate
For the currently [supported languages](#supported-languages) it seems to do a good job of finding what you want. If you find a case where it does not work as expected do not hesitate to [open an issue](https://github.com/zenspider/dumber-jump/issues). Although it can be sped up by creating a `.dumberjump` file in your project's root directory with paths that should be excluded ([see configuration](#configuration)).

## Supported Languages

There is currently basic support for the following languages:

* Apex
* Bash
* C/C++
* C#
* Clojure
* CoffeeScript
* Common Lisp
* Coq
* Crystal
* Dart
* Elixir
* Emacs Lisp
* Erlang
* F#
* Faust
* Fennel
* Fortran
* Go
* Groovy
* Haskell
* Java
* JavaScript
* Julia
* Kotlin
* LaTeX
* Lua
* Matlab
* Nim
* Nix
* Objective-C
* OCaml
* OpenSCAD
* Org mode
* Pascal
* Perl
* PHP
* Protocol Buffers
* Python
* R
* Racket
* Ruby
* Rust
* Sass
* Scala
* Scheme
* SML
* Solidity
* SQL
* Swift
* SystemVerilog
* Tcl
* Terraform / HCL
* TypeScript
* Vala
* VHDL
* Zig


If you have any issues with the existing languages, or you want support for another one, then please [open an issue](https://github.com/zenspider/dumber-jump/issues). PRs are also welcome. If you'd like to add a language these PRs for [lua](https://github.com/zenspider/dumber-jump/pull/33) and [rust](https://github.com/zenspider/dumber-jump/pull/57) are good examples.

## Installing

The recommended way to install Dumber Jump is via `package.el`. It's available on [MELPA](http://melpa.org/#/dumber-jump): <kbd>M-x</kbd> `package-install dumber-jump`

#### Installing `rg`

Dumber Jump requires ripgrep `rg` ([rg install instructions](https://github.com/BurntSushi/ripgrep#installation)) installed on your system.

## Usage

#### Basic

To enable the [xref][] backend, evaluate

~~~lisp
(add-hook 'xref-backend-functions #'dumber-jump-xref-activate)
~~~

or add it to your initialisation file. Using this, you can now use
<kbd>M-.</kbd> (or <kbd>gd</kbd> when using Evil).

[xref]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html

## Configuration

##### Excluding project directories

Dumber Jump will automatically look for a project root. If it's not finding one then either put a `.dumberjump` file in your project root and optionally add excluded directories to make it faster.

Project root directory denoters: `.dumberjump`, `.projectile` `.git` `.hg`, and more. See `dumber-jump-project-denoters`.

If you want to stop a directory from registering as the project root (and have Dumber Jump keep looking) add an empty `.dumbjumpignore` file in that directory.

##### Example `.dumberjump`

    -tests
    -node_modules
    -build
    -images
    +../some-lib/src
    +/usr/lib/src

##### `.emacs` options

* `(setq dumber-jump-default-project "~/code")` to change default project if one is not found (defaults to `~`)
* `(setq dumber-jump-quiet t)` if Dumber Jump is too chatty.
* To support more languages and/or definition types customize `dumber-jump-find-rules` variable.
* `(setq dumber-jump-rg-search-args "")` to set additional command line arguments when using rg for searching (defaults to `"--pcre2"`).

##### Hydra for effieciency

If you have [Hydra](https://github.com/abo-abo/hydra) installed, the following is an example hydra for easily using Dumber-Jump and not needing to remember the bindings or function names:

```el
(defhydra dumber-jump-hydra (:color blue :columns 3)
    "Dumber Jump"
    ("j" dumber-jump-go "Go")
    ("o" dumber-jump-go-other-window "Other window")
    ("e" dumber-jump-go-prefer-external "Go external")
    ("x" dumber-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumber-jump-go-prompt "Prompt")
    ("l" dumber-jump-quick-look "Quick look")
    ("b" dumber-jump-back "Back"))
```

It can be explicitly bound or used inside another hydra (if you already use something like [Avy](https://github.com/abo-abo/avy)/[Ace](https://github.com/winterTTr/ace-jump-mode) or similar for general "jumping").

#### Debugging a jump

1. <kbd>M-x</kbd> `set-variable dumber-jump-debug t`
1. try to jump
1. go to buffer `*Messages*`

More details [here](http://p.cweiske.de/506). Thanks to @cweiske and @Glumanda99

## Why?

I wanted "jump to definition" functionality to "just work" in emacs. I use IntelliJ for Java and this functionality is basically the only thing I miss when I switch back to emacs for work in other languages. There are certainly other packages that offer this type of functionality, but they all require significantly more configuration and are often limited to a particular language. An alternative may be worth setting up if you are in a specific project or language often (see [alternatives](#alternatives)).

## Contributing

Feedback is very welcome via GitHub issues. I will consider supporting other languages either via issue request or PR. If submitting a PR then please add tests as well.

## Running Tests

Opening a PR will use CircleCI to run all the tests against all the supported emacs versions and search programs.

### Running tests locally

There are a lot of options for running the tests locally:

#### Basic/Classic
requires [Cask](https://github.com/cask/cask) using your local emacs
```sh
cd /path/to/dumber-jump
cask
make test
```

#### Concurrent
requires golang and [Cask](https://github.com/cask/cask) using your local emacs
```sh
cd /path/to/dumber-jump
cask
make test-concurrent
```

#### Docker (latest emacs)
only requires docker and runs tests against emacs 26.1
```sh
cd /path/to/dumber-jump
cask
make test-in-docker
```

#### Docker (all supported emacs versions)
only requires docker and runs tests against all supported emacs versions
```sh
cd /path/to/dumber-jump
cask
make test-all-in-docker
```

## Alternatives

Here is a list of potential alternative packages for emacs:

* [Tags](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tags-Tables.html) supports multiple languages
* [GNU Global](http://www.gnu.org/software/global/) supports multiple languages
* [Tern](http://ternjs.net/) for JavaScript
* [elpy](https://github.com/jorgenschaefer/elpy) for Python
* [robe](https://github.com/dgutov/robe) for Ruby
* [dumb-jump] the original that this project is forked from. See that
  project's readme for what this used to be like.

Most of these were sourced from this [emacs StackExchange answer](http://emacs.stackexchange.com/questions/10125/can-emacs-support-go-to-declaration-of-function-in-an-entire-project).

[dumb-jump]: https://github.com/jacktasia/dumb-jump
