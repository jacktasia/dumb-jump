# dumb-jump

[![Join the chat at https://gitter.im/jacktasia/dumb-jump](https://badges.gitter.im/jacktasia/dumb-jump.svg)](https://gitter.im/jacktasia/dumb-jump?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/jacktasia/dumb-jump.svg)](https://travis-ci.org/jacktasia/dumb-jump) [![Coverage Status](https://coveralls.io/repos/jacktasia/dumb-jump/badge.svg?branch=master&service=github&x=1)](https://coveralls.io/github/jacktasia/dumb-jump?branch=master) [![MELPA](http://melpa.org/packages/dumb-jump-badge.svg?x=6)](http://melpa.org/#/dumb-jump) [![MELPA Stable](https://stable.melpa.org/packages/dumb-jump-badge.svg?x=1)](https://stable.melpa.org/#/dumb-jump)

![Dumb Jump GIF](media/dumb-jump-example-v2.gif?raw=true)

## About
**Dumb Jump** is an Emacs "jump to definition" package with support for multiple programming languages that favors "just working". This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (TAGS) or persistent background processes. Dumb Jump requires at least GNU Emacs `24.3`.


#### How  it works
Dumb Jump uses [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) `ag`, [ripgrep](https://github.com/BurntSushi/ripgrep) `rg`, or `grep` to find potential definitions of a function or variable under point. It uses a set of regular expressions based on the file extension, or `major-mode`, of the current buffer. The matches are run through a shared set of heuristic methods to find the best candidate to jump to. If it can't decide it will present the user with a list in a pop-menu.

#### Success Rate
For the currently [supported languages](#supported-languages) it seems to do a good job of finding what you want. If you find a case where it does not work as expected do not hesitate to [open an issue](https://github.com/jacktasia/dumb-jump/issues). It can be slow if it needs to use `grep` and/or a project is large. Although it can be sped up by [installing `ag`](https://github.com/ggreer/the_silver_searcher#installing) or [installing `rg`](https://github.com/BurntSushi/ripgrep#installation) and/or creating a `.dumbjump` file in your project's root directory with paths that should be excluded ([see configuration](#configuration)).


## Supported Languages

There is currently basic support for the following languages:

* Bash
* C/C++
* C#
* Clojure
* CoffeeScript
* Emacs Lisp
* Faust
* Fortran
* Go
* Haskell
* Java
* JavaScript
* Lua
* Objective-C
* Perl
* PHP
* Python
* R
* Ruby
* Rust
* Scala
* Swift

If you have any issues with the existing languages, or you want support for another one, then please [open an issue](https://github.com/jacktasia/dumb-jump/issues). PRs are also welcome. If you'd like to add a language these PRs for [lua](https://github.com/jacktasia/dumb-jump/pull/33) and [rust](https://github.com/jacktasia/dumb-jump/pull/57) are good examples.

## Installing

The recommended way to install Dumb Jump is via `package.el`. It's available on [MELPA](http://melpa.org/#/dumb-jump): <kbd>M-x</kbd> `package-install dumb-jump`

#### Spacemacs

If you're using an up-to-date Spacemacs, then you already have Dumb Jump by default just make sure you install `ag` or `rg` (see below) to ensure you have the best experience.


#### Installing `ag` or `rg`

Dumb Jump performs best with The Silver Searcher `ag` ([ag install instructions](https://github.com/ggreer/the_silver_searcher#installing)) or ripgrep `rg` ([rg install instructions](https://github.com/BurntSushi/ripgrep#installation)) installed on your system.

## Usage

#### Basic

Adding `(dumb-jump-mode)` to your `.emacs` will enable the key bindings for two interactive Dumb Jump functions:

* `dumb-jump-go` <kbd>C-M-g</kbd> core functionality. Attempts to jump to the definition for the thing under point
* `dumb-jump-back` <kbd>C-M-p</kbd> jumps back to where you were when you jumped. These are chained so if you go down a rabbit hole you can get back out or where you want to be.
* `dumb-jump-quick-look` <kbd>C-M-q</kbd> like `dumb-jump-go` but shows tooltip with `file`, `line`, and `context`
* `dumb-jump-go-other-window` exactly like `dumb-jump-go` but uses `find-file-other-window` instead of `find-file`
* `dumb-jump-go-prefer-external` like `dumb-jump-go` but will prefer definitions not in the current buffer
* `dumb-jump-go-prefer-external-other-window` expected combination of `dumb-jump-go-prefer-external` and `dumb-jump-go-other-window`
* `dumb-jump-go-prompt` exactly like `dumb-jump-go` but prompts user for function to jump to instead of using symbol at point

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

##### `.emacs` options

* `(setq dumb-jump-default-project "~/code")` to change default project if one is not found (defaults to `~`)
* `(setq dumb-jump-quiet t)` if Dumb Jump is too chatty.
* To support more languages and/or definition types use `add-to-list` on `dumb-jump-find-rules` (see source code).
* `(add-hook 'dumb-jump-after-jump-hook 'some-function)` to execute code after you jump
* `(setq dumb-jump-selector 'ivy)` to use [ivy](https://github.com/abo-abo/swiper#ivy) instead of the default popup for multiple options.
* `(setq dumb-jump-selector 'helm)` to use [helm](https://github.com/emacs-helm/helm) instead of the default popup for multiple options.
* `(setq dumb-jump-force-searcher 'rg)` to force the search program Dumb Jump should use. It will _always_ use this searcher. If not set (`nil`) Dumb Jump will use `git-grep` if it's a git project and if not will try searchers in the following order `ag`, `rg`, `grep` (first installed wins). This is necessary if you want full control over the searcher Dumb Jump uses.
* `(setq dumb-jump-aggressive nil)` to only automatically jump if there's only one match and otherwise present you with a list. This defaults to `t`, which means it will try its best to guess where you want to jump and only if it can't then give you a list of matches.
* `(setq dumb-jump-use-visible-window nil)` if `t` (the default) when you're using multiple windows/panes and the file to jump to is already open in one of those windows then dump jump will focus that window and jump there instead of within your current window.
* `(setq dumb-jump-prefer-searcher 'rg)` to let Dumb Jump know your searcher preference. If set this will still use `git-grep` if it's a git project (because it's the fastest), but will you use whatever you set here in any other situation. If not set Dumb Jump will follow the same order as mentioned in the `dumb-jump-force-searcher` description. At this time setting this value is only necessary if you prefer `rg` but have `ag` installed too.

**If your project has multi-line method signatures [you should use `ag`](https://github.com/jacktasia/dumb-jump/issues/129)**.

To learn more about how Dumb Jump picks a searcher see [this issue](https://github.com/jacktasia/dumb-jump/issues/109) and this [pull request](https://github.com/jacktasia/dumb-jump/pull/111).


##### `use-package` example configuration.

I personally no longer use the `dumb-jump-mode` keybindings that were inspired by IntelliJ's emacs bindings. I use `use-package` like so:

    (use-package dumb-jump
      :bind (("M-g o" . dumb-jump-go-other-window)
             ("M-g j" . dumb-jump-go)
             ("M-g i" . dumb-jump-go-prompt)
             ("M-g x" . dumb-jump-go-prefer-external)
             ("M-g z" . dumb-jump-go-prefer-external-other-window))
      :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
      :ensure)

## Why?

I wanted "jump to definition" functionality to "just work" in emacs. I use IntelliJ for Java and this functionality is basically the only thing I miss when I switch back to emacs for work in other languages. There are certainly other packages that offer this type of functionality, but they all require significantly more configuration and are often limited to a particular language. An alternative may be worth setting up if you are in a specific project or language often (see [alternatives](#alternatives)).

## Contributing

Feedback is very welcome via GitHub issues. I will consider supporting other languages either via issue request or PR. If submitting a PR then please add tests as well.

## Running Tests
Requires [Cask](https://github.com/cask/cask).

    cd /path/to/dumb-jump
    cask
    make test

## Alternatives

Here is a list of potential alternative packages for emacs:

* [Tags](http://www.gnu.org/software/emacs/manual/html_node/emacs/Tags.html) supports multiple languages
* [GNU Global](http://www.gnu.org/software/global/) supports multiple languages
* [Tern](http://ternjs.net/) for JavaScript
* [elpy](https://github.com/jorgenschaefer/elpy) for Python
* [robe](https://github.com/dgutov/robe) for Ruby

Most of these were sourced from this [emacs StackExchange answer](http://emacs.stackexchange.com/questions/10125/can-emacs-support-go-to-declaration-of-function-in-an-entire-project).
