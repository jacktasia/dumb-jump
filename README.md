# dumb-jump

![CI](https://github.com/jacktasia/dumb-jump/workflows/CI/badge.svg) [![Coverage Status](https://coveralls.io/repos/jacktasia/dumb-jump/badge.svg?branch=master&service=github&x=1)](https://coveralls.io/github/jacktasia/dumb-jump?branch=master) [![MELPA](http://melpa.org/packages/dumb-jump-badge.svg?x=6)](http://melpa.org/#/dumb-jump) [![MELPA Stable](https://stable.melpa.org/packages/dumb-jump-badge.svg?x=1)](https://stable.melpa.org/#/dumb-jump)

![Dumb Jump GIF](media/dumb-jump-example-v2.gif?raw=true)

## About
**Dumb Jump** is an Emacs "jump to definition" package with support for 50+ programming languages that favors "just working". This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (TAGS) or persistent background processes. Dumb Jump requires at least GNU Emacs `24.3`.


#### How  it works
Dumb Jump uses [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) `ag`, [ripgrep](https://github.com/BurntSushi/ripgrep) `rg`, or `grep` to find potential definitions of a function or variable under point. It uses a set of regular expressions based on the file extension, or `major-mode`, of the current buffer. The matches are run through a shared set of heuristic methods to find the best candidate to jump to. If it can't decide it will present the user with a list in a pop-menu, helm, or ivy (see `dumb-jump-selector`).

#### Success Rate
For the currently [supported languages](#supported-languages) it seems to do a good job of finding what you want. If you find a case where it does not work as expected do not hesitate to [open an issue](https://github.com/jacktasia/dumb-jump/issues). It can be slow if it needs to use `grep` and/or a project is large. Although it can be sped up by [installing `ag`](https://github.com/ggreer/the_silver_searcher#installing) or [installing `rg`](https://github.com/BurntSushi/ripgrep#installation) and/or creating a `.dumbjump` file in your project's root directory with paths that should be excluded ([see configuration](#configuration)).


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


If you have any issues with the existing languages, or you want support for another one, then please [open an issue](https://github.com/jacktasia/dumb-jump/issues). PRs are also welcome. If you'd like to add a language these PRs for [lua](https://github.com/jacktasia/dumb-jump/pull/33) and [rust](https://github.com/jacktasia/dumb-jump/pull/57) are good examples.

## Installing

The recommended way to install Dumb Jump is via `package.el`. It's available on [MELPA](http://melpa.org/#/dumb-jump): <kbd>M-x</kbd> `package-install dumb-jump`

#### Spacemacs

If you're using an up-to-date Spacemacs, then you already have Dumb Jump by default just make sure you install `ag` or `rg` (see below) to ensure you have the best experience.


#### Installing `ag` or `rg`

Dumb Jump performs best with The Silver Searcher `ag` ([ag install instructions](https://github.com/ggreer/the_silver_searcher#installing)) or ripgrep `rg` ([rg install instructions](https://github.com/BurntSushi/ripgrep#installation)) installed on your system.

## Usage

#### Basic

To enable the [xref][] backend, evaluate

~~~lisp
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
~~~

or add it to your initialisation file. Using this, you can now use
<kbd>M-.</kbd> (or <kbd>gd</kbd> when using Evil).

Xref can be customized to use `completing-read` to select a
target. That way a completion framework of your choice (Icomplete,
Helm, Ivy, ...) will be used instead of the default pop-up buffer. To
do this, evaluate

~~~lisp
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
~~~

Note that the function `xref-show-definitions-completing-read`
requires at least Xref 1.1.0. This can either be downloaded from ELPA
or is bundled with Emacs 28.1 or newer.

[xref]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html

## Configuration

##### Excluding project directories

Dumb Jump will automatically look for a project root. If it's not finding one then either put a `.dumbjump` file in your project root and optionally add excluded directories to make it faster.

Project root directory denoters: `.dumbjump` `.projectile` `.git` `.hg` `.fslckout` `.bzr` `_darcs` `.svn` `Makefile` `PkgInfo` `-pkg.el`.

If you want to stop a directory from registering as the project root (and have Dumb Jump keep looking) add an empty `.dumbjumpignore` file in that directory.

##### Example `.dumbjump`

    -tests
    -node_modules
    -build
    -images
    +../some-lib/src
    +/usr/lib/src

*NOTE* When adding paths outside of the project (using `+`) ensure you use `dumb-jump-force-searcher` of either `'ag` or `'rg` (see below). This is required because the default searcher (`git-grep`) won't be able to search outside of the project root. This edge case will be fixed in a future release. That is, `git-grep` will NOT be set as the default searcher if a `.dumbjump` is present with a `+` path outside of the repo.

##### `.emacs` options

* `(setq dumb-jump-default-project "~/code")` to change default project if one is not found (defaults to `~`)
* `(setq dumb-jump-quiet t)` if Dumb Jump is too chatty.
* To support more languages and/or definition types customize `dumb-jump-find-rules` variable.
* `(setq dumb-jump-force-searcher 'rg)` to force the search program Dumb Jump should use. It will _always_ use this searcher. If not set (`nil`) Dumb Jump will use `git-grep` if it's a git project and if not will try searchers in the following order `ag`, `rg`, `grep` (first installed wins). This is necessary if you want full control over the searcher Dumb Jump uses.
* `(setq dumb-jump-prefer-searcher 'rg)` to let Dumb Jump know your searcher preference. If set this will still use `git-grep` if it's a git project (because it's the fastest), but will you use whatever you set here in any other situation. If not set Dumb Jump will follow the same order as mentioned in the `dumb-jump-force-searcher` description. At this time setting this value is only necessary if you prefer `rg` but have `ag` installed too.
* `(setq dumb-jump-git-grep-search-args "")` to set additional command line arguments when using git-grep for searching (defaults to `""`).
* `(setq dumb-jump-ag-search-args "")` to set additional command line arguments when using ag for searching (defaults to `""`).
* `(setq dumb-jump-rg-search-args "")` to set additional command line arguments when using rg for searching (defaults to `"--pcre2"`).

#### If your project has multi-line method signatures [you should use `ag`](https://github.com/jacktasia/dumb-jump/issues/129) or [`rg` version `0.10.0` or higher](https://github.com/jacktasia/dumb-jump/issues/255).

To learn more about how Dumb Jump picks a searcher see [this issue](https://github.com/jacktasia/dumb-jump/issues/109) and this [pull request](https://github.com/jacktasia/dumb-jump/pull/111).

##### Hydra for effieciency

If you have [Hydra](https://github.com/abo-abo/hydra) installed, the following is an example hydra for easily using Dumb-Jump and not needing to remember the bindings or function names:

```el
(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
```

It can be explicitly bound or used inside another hydra (if you already use something like [Avy](https://github.com/abo-abo/avy)/[Ace](https://github.com/winterTTr/ace-jump-mode) or similar for general "jumping").

#### Debugging a jump

1. <kbd>M-x</kbd> `set-variable dumb-jump-debug t`
1. try to jump
1. go to buffer `*Messages*`

More details [here](http://p.cweiske.de/506). Thanks to @cweiske and @Glumanda99

## Obsolete commands and options

Older versions of dumb jump didn't use xref, and instead had custom
commands. These, while marked obsolete, can still be used:

* `dumb-jump-go` (former) core functionality. Attempts to jump to the
  definition for the thing under point. This has been replaced in the
  new interface with `xref-find-definitions` (<kbd>M-.</kbd>).
* `dumb-jump-back` jumps back to where you were when you jumped. These
  are chained so if you go down a rabbit hole you can get back out or
  where you want to be. This has been replaced with
  `xref-pop-marker-stack` (<kbd>M-,</kbd>), but is mostly equivalent.
* `dumb-jump-quick-look` like `dumb-jump-go` but **only** shows
  tooltip with `file`, `line`, and `context` it does not jump.
* `dumb-jump-go-other-window` exactly like `dumb-jump-go` but uses
  `find-file-other-window` instead of `find-file`
* `dumb-jump-go-prefer-external` like `dumb-jump-go` but will prefer
  definitions not in the current buffer
* `dumb-jump-go-prefer-external-other-window` expected combination of
  `dumb-jump-go-prefer-external` and `dumb-jump-go-other-window`
* `dumb-jump-go-prompt` exactly like `dumb-jump-go` but prompts user
  for function to jump to

A few user options only have an effect when used with the legacy
interface. These are:

* `dumb-jump-after-jump-hook` (use `xref-after-jump-hook` instead)
* `dumb-jump-before-jump-hook` (use `xref-after-return-hook` instead)
* `dumb-jump-selector`
* `dumb-jump-aggressive`
* `dumb-jump-use-visible-window`
* `dumb-jump-confirm-jump-to-modified-file`

The minor mode `dumb-jump-mode` binds a few of these commands by
default.

If you still use Emacs 24 or older, you won't have xref, and have to
use the legacy interface instead. In that case, there will also be no
"obsolete" warnings.

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
cd /path/to/dumb-jump
cask
make test
```

#### Concurrent
requires golang and [Cask](https://github.com/cask/cask) using your local emacs
```sh
cd /path/to/dumb-jump
cask
make test-concurrent
```

#### Docker (latest emacs)
only requires docker and runs tests against emacs 26.1
```sh
cd /path/to/dumb-jump
cask
make test-in-docker
```

#### Docker (all supported emacs versions)
only requires docker and runs tests against all supported emacs versions
```sh
cd /path/to/dumb-jump
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

Most of these were sourced from this [emacs StackExchange answer](http://emacs.stackexchange.com/questions/10125/can-emacs-support-go-to-declaration-of-function-in-an-entire-project).
