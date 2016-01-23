# dumb-jump

[![Join the chat at https://gitter.im/jacktasia/dumb-jump](https://badges.gitter.im/jacktasia/dumb-jump.svg)](https://gitter.im/jacktasia/dumb-jump?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/jacktasia/dumb-jump.svg)](https://travis-ci.org/jacktasia/dumb-jump) [![Coverage Status](https://coveralls.io/repos/jacktasia/dumb-jump/badge.svg?branch=master&service=github)](https://coveralls.io/github/jacktasia/dumb-jump?branch=master)

## About
**Dumb Jump** is an Emacs "jump to definition" package with support for multiple programming languages that favors "just working" over speed or accuracy. This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (CTAGS,ETAGS) or persistent background processes.

Dumb Jump uses `grep` <sup id="a1">[1](#f1)</sup> (see why grep) to find potential definitions of a variable/function (under point) via a set of regular expressions based on the file extension of the current buffer. Once there is a list of potential candidates they are run through a set of heuristic methods to find the best candidate to jump to. Right now these methods are general and shared among languages but in the future will almost certainly have to be customized to increase accuracy and/or support more languages.

It **seems** like in most situations for JavaScript, Emacs Lisp, Python, and go it does a good job of finding what you want. It can be slow if a project is very large, but it can usually be sped up quickly by creating a `.dumbjump` file in your project's root directory with paths that should be excluded (see configuration).

[GIF]

<b id="f1">1</b> I could see support for ack or ag support instead, but `grep` is pretty much everywhere so I felt it made sense to go first. [↩](#a1)

## Why?

I wanted "jump to definition" functionality to "just work" in emacs. I use IntelliJ for Java and it's basically the only thing I miss when I switch back to emacs for work in any other language. There are certainly other packages that offer this type of functionality and honestly many are faster and have better accuracy but they all require significantly more configuration and are often limited to a particular language. These may be worth setting up if you are in a certain project or language often.

Here is a list of potential alternatives packages for emacs:

## Installing

## Configuration

## Contributing
