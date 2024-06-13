;;; dumber-jump.el --- Jump to definition for 50+ languages without configuration -*- lexical-binding: t; byte-compile-docstring-max-column: 999; -*-
;; Copyright (C) 2015-2021 jack angers
;; Author: jack angers and contributors
;; Url: https://github.com/zenspider/dumber-jump
;; Version: 0.5.4
;; Package-Requires: ((emacs "28.1") (s "1.11.0") (dash "2.9.0"))
;; Keywords: tools

;; Dumber Jump is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Dumber Jump is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Dumber Jump.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Dumber Jump is an Emacs "jump to definition" package with support for 50+ programming languages that favors
;; "just working" over speed or accuracy.  This means minimal -- and ideally zero -- configuration with absolutely
;; no stored indexes (TAGS) or persistent background processes.
;;
;; Dumber Jump provides a xref-based interface for jumping to
;; definitions. It uses ripgrep
;; (https://github.com/BurntSushi/ripgrep) for all searching.
;;
;; To enable Dumber Jump, add the following to your initialisation file:
;;
;;    (add-hook 'xref-backend-functions #'dumber-jump-xref-activate)
;;
;; Now pressing M-. on an identifier should open a buffer at the place
;; where it is defined, or a list of candidates if uncertain. This
;; list can be navigated using M-g M-n (next-error) and M-g M-p
;; (previous-error).

;;; Code:
(require 'xref)
(require 's)
(require 'dash)
(require 'cl-generic nil :noerror)
(require 'cl-lib)
(require 'tramp)

(defgroup dumber-jump nil
  "Easily jump to project function and variable definitions."
  :group 'tools
  :group 'convenience)

(defcustom dumber-jump-rg-cmd
  "rg"
  "The the path to ripgrep.  By default assumes it is in path."
  :group 'dumber-jump
  :type 'string)

(defcustom dumber-jump-rg-word-boundary
  "($|[^a-zA-Z0-9\\?\\*-])"
  "`\\b` thinks `-` is a word boundary.  When this matters use `\\j` instead and rg will use this value."
  :group 'dumber-jump
  :type 'string)

(defcustom dumber-jump-fallback-regex
  "\\bJJJ\\j"
  "When `dumber-jump-fallback-search' is t use this regex.  Defaults to boundary search of symbol under point."
  :group 'dumber-jump
  :type 'string)

(defcustom dumber-jump-fallback-search  ; TODO: remove?
  t
  "If nothing is found with normal search fallback to searching the fallback regex."
  :group 'dumber-jump
  :type 'boolean)

(defcustom dumber-jump-functions-only
  nil
  "Should we only jump to functions?"
  :group 'dumber-jump
  :type 'boolean)

(defcustom dumber-jump-quiet
  nil
  "If non-nil Dumber Jump will not log anything to *Messages*."
  :group 'dumber-jump
  :type 'boolean)

(defcustom dumber-jump-ignore-context
  nil
  "If non-nil Dumber Jump will ignore the context of point when jumping."
  :group 'dumber-jump
  :type 'boolean)

(defcustom dumber-jump-rg-search-args
  "--pcre2"
  "Appends the passed arguments to the rg search function. Default: \"--pcre2\"."
  :group 'dumber-jump
  :type 'string)

;; TODO: remove :supports
(defcustom dumber-jump-find-rules
  '((:type "function" :supports ("rg") :language "elisp"
           :regex "\\\((defun|cl-defun)\\s+JJJ\\j"
           ;; \\j usage see `dumber-jump-rg-word-boundary`
           :tests ("(defun test (blah)" "(defun test\n" "(cl-defun test (blah)" "(cl-defun test\n")
           :not ("(defun test-asdf (blah)" "(defun test-blah\n" "(cl-defun test-asdf (blah)"
                 "(cl-defun test-blah\n"  "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))

    (:type "function" :supports ("rg") :language "elisp"
           :regex "\\\(defmacro\\s+JJJ\\j"
           :tests ("(defmacro test (blah)" "(defmacro test\n")
           :not ("(defmacro test-asdf (blah)" "(defmacro test-blah\n" "(defmacro tester (blah)"
                 "(defmacro test? (blah)" "(defmacro test- (blah)"))

    (:type "variable" :supports ("rg") :language "elisp"
           :regex "\\\(defvar\\b\\s*JJJ\\j"
           :tests ("(defvar test " "(defvar test\n")
           :not ("(defvar tester" "(defvar test?" "(defvar test-"))

    (:type "variable" :supports ("rg") :language "elisp"
           :regex "\\\(defcustom\\b\\s*JJJ\\j"
           :tests ("(defcustom test " "(defcustom test\n")
           :not ("(defcustom tester" "(defcustom test?" "(defcustom test-"))

    (:type "variable" :supports ("rg") :language "elisp"
           :regex "\\\(setq\\b\\s*JJJ\\j" :tests ("(setq test 123)")
           :not ("setq test-blah 123)" "(setq tester" "(setq test?" "(setq test-"))

    (:type "variable" :supports ("rg") :language "elisp"
           :regex "\\\(JJJ\\s+" :tests ("(let ((test 123)))") :not ("(let ((test-2 123)))"))

    ;; variable in method signature
    (:type "variable" :supports ("rg") :language "elisp"
           :regex "\\((defun|cl-defun)\\s*.+\\\(?\\s*JJJ\\j\\s*\\\)?"
           :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)")
           :not ("(defun blah (test-1)" "(defun blah (test-2 blah)" "(defun (blah test-3)"))

    ;; common lisp
    (:type "function" :supports ("rg") :language "commonlisp"
           :regex "\\\(defun\\s+JJJ\\j"
           ;; \\j usage see `dumber-jump-rg-word-boundary`
           :tests ("(defun test (blah)" "(defun test\n")
           :not ("(defun test-asdf (blah)" "(defun test-blah\n"
                 "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))

    (:type "variable" :supports ("rg") :language "commonlisp"
           :regex "\\\(defparameter\\b\\s*JJJ\\j"
           :tests ("(defparameter test " "(defparameter test\n")
           :not ("(defparameter tester" "(defparameter test?" "(defparameter test-"))

    ;; racket
    (:type "function" :supports ("rg") :language "racket"
           :regex "\\\(define\\s+\\(\\s*JJJ\\j"
           :tests ("(define (test blah)" "(define (test\n")
           :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))

    (:type "function" :supports ("rg") :language "racket"
           :regex "\\\(define\\s+JJJ\\s*\\\(\\s*lambda"
           :tests ("(define test (lambda (blah" "(define test (lambda\n")
           :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))

    (:type "function" :supports ("rg") :language "racket"
           :regex "\\\(let\\s+JJJ\\s*(\\\(|\\\[)*"
           :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
           :not ("(let ((test blah"))

    (:type "variable" :supports ("rg") :language "racket"
           :regex "\\\(define\\s+JJJ\\j"
           :tests ("(define test " "(define test\n")
           :not ("(define (test"))

    (:type "variable" :supports ("rg") :language "racket"
           :regex "(\\\(|\\\[)\\s*JJJ\\s+"
           :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
           :not ("{test foo"))

    (:type "variable" :supports ("rg") :language "racket"
           :regex "\\\(lambda\\s+\\\(?[^\(\)]*\\s*JJJ\\j\\s*\\\)?"
           :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
           :not ("(lambda () test"))

    (:type "variable" :supports ("rg") :language "racket"
           :regex "\\\(define\\s+\\\([^\(\)]+\\s*JJJ\\j\\s*\\\)?"
           :tests ("(define (foo test)" "(define (foo test bar)")
           :not ("(define foo test" "(define (test foo" "(define (test)"))

    (:type "type" :supports ("rg") :language "racket"
           :regex "\\(struct\\s+JJJ\\j"
           :tests ("(struct test (a b)"))

    ;; scheme
    (:type "function" :supports ("rg") :language "scheme"
           :regex "\\\(define\\s+\\(\\s*JJJ\\j"
           :tests ("(define (test blah)" "(define (test\n")
           :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))

    (:type "function" :supports ("rg") :language "scheme"
           :regex "\\\(define\\s+JJJ\\s*\\\(\\s*lambda"
           :tests ("(define test (lambda (blah" "(define test (lambda\n")
           :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))

    (:type "function" :supports ("rg") :language "scheme"
           :regex "\\\(let\\s+JJJ\\s*(\\\(|\\\[)*"
           :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
           :not ("(let ((test blah"))

    (:type "variable" :supports ("rg") :language "scheme"
           :regex "\\\(define\\s+JJJ\\j"
           :tests ("(define test " "(define test\n")
           :not ("(define (test"))

    (:type "variable" :supports ("rg") :language "scheme"
           :regex "(\\\(|\\\[)\\s*JJJ\\s+"
           :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
           :not ("{test foo"))

    (:type "variable" :supports ("rg") :language "scheme"
           :regex "\\\(lambda\\s+\\\(?[^\(\)]*\\s*JJJ\\j\\s*\\\)?"
           :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
           :not ("(lambda () test"))

    (:type "variable" :supports ("rg") :language "scheme"
           :regex "\\\(define\\s+\\\([^\(\)]+\\s*JJJ\\j\\s*\\\)?"
           :tests ("(define (foo test)" "(define (foo test bar)")
           :not ("(define foo test" "(define (test foo" "(define (test)"))

    ;; janet
    (:type "variable" :supports ("rg") :language "janet"
           :regex "\\(\(de\)?f\\s+JJJ\\j"
           :tests ("(def test (foo)"))

    (:type "variable" :supports ("rg") :language "janet"
           :regex "\\(var\\s+JJJ\\j"
           :tests ("(var test (foo)"))

    (:type "function" :supports ("rg") :language "janet"
           :regex "\\(\(de\)fn-?\\s+JJJ\\j"
           :tests ("(defn test [foo]" "(defn- test [foo]")
           :not ("(defn test? [foo]" "(defn- test? [foo]"))

    (:type "function" :supports ("rg") :language "janet"
           :regex "\\(defmacro\\s+JJJ\\j"
           :tests ("(defmacro test [foo]"))

    ;; c++
    (:type "function" :supports ("rg") :language "c++"
           :regex "\\bJJJ(\\s|\\))*\\((\\w|[,&*.<>:]|\\s)*(\\))\\s*(const|->|\\{|$)|typedef\\s+(\\w|[(*]|\\s)+JJJ(\\)|\\s)*\\("
           :tests ("int test(){" "my_struct (*test)(int a, int b){" "auto MyClass::test ( Builder::Builder& reference, ) -> decltype( builder.func() ) {" "int test( int *random_argument) const {" "test::test() {" "typedef int (*test)(int);")
           :not ("return test();)" "int test(a, b);" "if( test() ) {" "else test();"))

    (:type "variable" :supports ("ag" "rg") :language "c++"
           :regex "\\b(?!(class\\b|struct\\b|return\\b|else\\b|delete\\b))(\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[(\\d|\\s)*\\])*\\s*([=,(){;]|:\\s*\\d)|#define\\s+JJJ\\b"
           :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "typedef int test;" "unsigned int test:2")
           :not ("return test;" "#define NOT test" "else test=2;"))

    (:type "type" :supports ("rg") :language "c++"
           :regex "\\b(class|struct|enum|union)\\b\\s*JJJ\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*JJJ\\b\\s*;"
           :tests ("typedef struct test {" "enum test {" "} test;" "union test {" "class test final: public Parent1, private Parent2{" "class test : public std::vector<int> {")
           :not("union test var;" "struct test function() {"))

    ;; clojure
    (:type "variable" :supports ("rg") :language "clojure"
           :regex "\\(def.*\ JJJ\\j"
           :tests ("(def test (foo)"
                   "(defn test [foo]"
                   "(defn ^:some-data test [foo]"
                   "(defn- test [foo]"
                   "(defmacro test [foo]"
                   "(deftask test [foo]"
                   "(deftype test [foo]"
                   "(defmulti test fn"
                   "(defmethod test type"
                   "(definterface test (foo)"
                   "(defprotocol test (foo)"
                   "(defrecord test [foo]"
                   "(deftest test"))

    ;; coffeescript
    (:type "function" :supports ("rg") :language "coffeescript"
           :regex "^\\s*JJJ\\s*[=:].*[-=]>"
           :tests ("test = ()  =>" "test= =>" "test = ->" "test=()->"
                   "test : ()  =>" "test: =>" "test : ->" "test:()->")
           :not ("# test = =>" "test = 1"))

    (:type "variable" :supports ("rg") :language "coffeescript"
           :regex "^\\s*JJJ\\s*[:=][^:=-][^>]+$"
           :tests ("test = $" "test : [" "test = {" "test = a")
           :not ("test::a" "test: =>" "test == 1" "# test = 1"))

    (:type "class" :supports ("rg") :language "coffeescript"
           :regex "^\\s*\\bclass\\s+JJJ"
           :tests ("class test" "class test extends")
           :not ("# class"))

    ;; obj-c
    (:type "function" :supports ("rg") :language "objc"
           :regex "\\\)\\s*JJJ(:|\\b|\\s)"
           :tests ("- (void)test" "- (void)test:(UIAlertView *)alertView")
           :not ("- (void)testnot" "- (void)testnot:(UIAlertView *)alertView"))

    (:type "variable" :supports ("rg") :language "objc"
           :regex "\\b\\*?JJJ\\s*=[^=\\n]+"
           :tests ("NSString *test = @\"asdf\"")
           :not ("NSString *testnot = @\"asdf\"" "NSString *nottest = @\"asdf\""))

    (:type "type" :supports ("rg") :language "objc"
           :regex "(@interface|@protocol|@implementation)\\b\\s*JJJ\\b\\s*"
           :tests ("@interface test: UIWindow")
           :not ("@interface testnon: UIWindow"))


    (:type "type" :supports ("rg") :language "objc"
           :regex "typedef\\b\\s+(NS_OPTIONS|NS_ENUM)\\b\\([^,]+?,\\s*JJJ\\b\\s*"
           :tests ("typedef NS_ENUM(NSUInteger, test)")
           :not ("typedef NS_ENUMD(NSUInteger, test)"))

    ;; swift
    (:type "variable" :supports ("rg") :language "swift"
           :regex "(let|var)\\s*JJJ\\s*(=|:)[^=:\\n]+"
           :tests ("let test = 1234" "var test = 1234" "private lazy var test: UITapGestureRecognizer")
           :not ("if test == 1234:"))

    (:type "function" :supports ("rg") :language "swift"
           :regex "func\\s+JJJ\\b\\s*(<[^>]*>)?\\s*\\("
           :tests ("func test(asdf)" "func test()" "func test<Value: Protocol>()")
           :not ("func testnot(asdf)" "func testnot()"))

    (:type "type" :supports ("rg") :language "swift"
           :regex "(class|struct|protocol|enum)\\s+JJJ\\b\\s*?"
           :tests ("struct test" "struct test: Codable" "struct test<Value: Codable>"
                   "class test:" "class test: UIWindow" "class test<Value: Codable>")
           :not ("class testnot:" "class testnot(object):" "struct testnot(object)"))

    (:type "type" :supports ("rg") :language "swift"
           :regex "(typealias)\\s+JJJ\\b\\s*?="
           :tests ("typealias test =")
           :not ("typealias testnot"))

    ;; c#
    (:type "function" :supports ("ag" "rg") :language "csharp"
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                   "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
           :not ("test()" "testnot()" "blah = new test()"))

    (:type "variable" :supports ("rg") :language "csharp"
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

    (:type "type" :supports ("rg") :language "csharp"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:" "public class test : IReadableChannel, I")
           :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; java (literally the same regexes as c#, but different tests)
    (:type "function" :supports ("ag" "rg") :language "java"
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                   "public static MyType test()" "private virtual SomeType test(param)" "static int test()"
                   "private foo[] test()")
           :not ("test()" "testnot()" "blah = new test()" "foo bar = test()"))

    (:type "variable" :supports ("rg") :language "java"
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

    (:type "type" :supports ("rg") :language "java"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:" "public class test implements Something")
           :not ("class testnot:" "public class testnot implements Something"))

    ;; vala (again just like c#, exactly the same..)
    (:type "function" :supports ("ag" "rg") :language "vala"
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                   "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
           :not ("test()" "testnot()" "blah = new test()"))

    (:type "variable" :supports ("rg") :language "vala"
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

    (:type "type" :supports ("rg") :language "vala"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:" "public class test : IReadableChannel, I")
           :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; coq
    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Variable\\s+JJJ\\b"
           :tests ("Variable test")
           :not ("Variable testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Inductive\\s+JJJ\\b"
           :tests ("Inductive test")
           :not ("Inductive testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Lemma\\s+JJJ\\b"
           :tests ("Lemma test")
           :not ("Lemma testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Definition\\s+JJJ\\b"
           :tests ("Definition test")
           :not ("Definition testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Hypothesis\\s+JJJ\\b"
           :tests ("Hypothesis test")
           :not ("Hypothesis testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Theorm\\s+JJJ\\b"
           :tests ("Theorm test")
           :not ("Theorm testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Fixpoint\\s+JJJ\\b"
           :tests ("Fixpoint test")
           :not ("Fixpoint testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*Module\\s+JJJ\\b"
           :tests ("Module test")
           :not ("Module testx"))

    (:type "function" :supports ("rg") :language "coq"
           :regex "\\s*CoInductive\\s+JJJ\\b"
           :tests ("CoInductive test")
           :not ("CoInductive testx"))

    ;; python
    (:type "variable" :supports ("rg") :language "python"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234:" "_test = 1234"))

    (:type "function" :supports ("rg") :language "python"
           :regex "def\\s*JJJ\\b\\s*\\\("
           :tests ("\tdef test(asdf)" "def test()")
           :not ("\tdef testnot(asdf)" "def testnot()"))

    (:type "type" :supports ("rg") :language "python"
           :regex "class\\s*JJJ\\b\\s*\\\(?"
           :tests ("class test(object):" "class test:")
           :not ("class testnot:" "class testnot(object):"))

    ;; matlab
    (:type "variable" :supports ("rg") :language "matlab"
           :regex "^\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("for test = 1:2:" "_test = 1234"))

    (:type "function" :supports ("rg") :language "matlab"
           :regex "^\\s*function\\s*[^=]+\\s*=\\s*JJJ\\b"
           :tests ("\tfunction y = test(asdf)" "function x = test()" "function [x, losses] = test(A, y, lambda, method, qtile)")
           :not ("\tfunction testnot(asdf)" "function testnot()"))

    (:type "type" :supports ("rg") :language "matlab"
           :regex "^\\s*classdef\\s*JJJ\\b\\s*"
           :tests ("classdef test")
           :not ("classdef testnot"))

    ;; nim
    (:type "variable" :supports ("rg") :language "nim"
           :regex "(const|let|var)\\s*JJJ\\*?\\s*(=|:)[^=:\\n]+"
           :tests ("let test = 1234" "var test = 1234" "var test: Stat" "const test = 1234" "const test* = 1234")
           :not ("if test == 1234:"))

    (:type "function" :supports ("rg") :language "nim"
           :regex "(proc|func|macro|template)\\s*`?JJJ`?\\b\\*?\\s*\\\("
           :tests ("\tproc test(asdf)" "proc test()" "func test()" "macro test()" "template test()" "proc test*()")
           :not ("\tproc testnot(asdf)" "proc testnot()"))

    (:type "type" :supports ("rg") :language "nim"
           :regex "type\\s*JJJ\\b\\*?\\s*(\\{[^}]+\\})?\\s*=\\s*\\w+"
           :tests ("type test = object" "type test {.pure.} = enum" "type test* = ref object")
           :not ("type testnot = object"))

    ;; nix
    (:type "variable" :supports ("rg") :language "nix"
           :regex "\\b\\s*JJJ\\s*=[^=;]+"
           :tests ("test = 1234;" "test = 123;" "test=123")
           :not ("testNot = 1234;" "Nottest = 1234;" "AtestNot = 1234;"))

    ;; ruby
    (:type "variable" :supports ("rg") :language "ruby"
           :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
           :tests ("test = 1234" "self.foo, test, bar = args")
           :not ("if test == 1234" "foo_test = 1234"))

    (:type "function" :supports ("rg") :language "ruby"
           :regex "(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
           :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
                   "def self.test()" "def MODULE::test()" "private def test")
           :not ("def test_foo"))

    (:type "function" :supports ("rg") :language "ruby"
           :regex "(^|\\W)define(_singleton|_instance)?_method(\\s|[(])\\s*:JJJ($|[^\\w|:])"
           :tests ("define_method(:test, &body)"
                   "mod.define_instance_method(:test) { body }"))

    (:type "type" :supports ("rg") :language "ruby"
           :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("class test" "class Foo::test"))

    (:type "type" :supports ("rg") :language "ruby"
           :regex "(^|[^\\w.])module\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("module test" "module Foo::test"))

    (:type "function" :supports ("rg") :language "ruby"
           :regex "(^|\\W)alias(_method)?\\W+JJJ(\\W|$)"
           :tests ("alias test some_method"
                   "alias_method :test, :some_method"
                   "alias_method 'test' 'some_method'"
                   "some_class.send(:alias_method, :test, :some_method)")
           :not ("alias some_method test"
                 "alias_method :some_method, :test"
                 "alias test_foo test"))

    ;; Groovy
    (:type "variable" :supports ("rg") :language "groovy"
           :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
           :tests ("test = 1234" "self.foo, test, bar = args")
           :not ("if test == 1234" "foo_test = 1234"))

    (:type "function" :supports ("rg") :language "groovy"
           :regex "(^|[^\\w.])((private|public)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
           :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
                   "def self.test()" "def MODULE::test()" "private def test")
           :not ("def test_foo"))

    (:type "type" :supports ("rg") :language "groovy"
           :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("class test" "class Foo::test"))

    ;; crystal
    (:type "variable" :supports ("rg") :language "crystal"
           :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
           :tests ("test = 1234" "self.foo, test, bar = args")
           :not ("if test == 1234" "foo_test = 1234"))

    (:type "function" :supports ("rg") :language "crystal"
           :regex "(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
           :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
                   "def self.test()" "def MODULE::test()" "private def test")
           :not ("def test_foo"))

    (:type "type" :supports ("rg") :language "crystal"
           :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("class test" "class Foo::test"))

    (:type "type" :supports ("rg") :language "crystal"
           :regex "(^|[^\\w.])module\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("module test" "module Foo::test"))

    (:type "type" :supports ("rg") :language "crystal"
           :regex "(^|[^\\w.])struct\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("struct test" "struct Foo::test"))

    (:type "type" :supports ("rg") :language "crystal"
           :regex "(^|[^\\w.])alias\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("alias test" "alias Foo::test"))

    ;; scad
    (:type "variable" :supports ("rg") :language "scad"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test == 1234 {"))

    (:type "function" :supports ("rg") :language "scad"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "module" :supports ("rg") :language "scad"
           :regex "module\\s*JJJ\\s*\\\("
           :tests ("module test()" "module test ()"))

    ;; scala
    (:type "variable" :supports ("rg") :language "scala"
           :regex "\\bval\\s*JJJ\\s*=[^=\\n]+" :tests ("val test = 1234") :not ("case test => 1234"))

    (:type "variable" :supports ("rg") :language "scala"
           :regex "\\bvar\\s*JJJ\\s*=[^=\\n]+" :tests ("var test = 1234") :not ("case test => 1234"))

    (:type "variable" :supports ("rg") :language "scala"
           :regex "\\btype\\s*JJJ\\s*=[^=\\n]+" :tests ("type test = 1234") :not ("case test => 1234"))

    (:type "function" :supports ("rg") :language "scala"
           :regex "\\bdef\\s*JJJ\\s*\\\("
           :tests ("def test(asdf)" "def test()"))

    (:type "type" :supports ("rg") :language "scala"
           :regex "class\\s*JJJ\\s*\\\(?"
           :tests ("class test(object)"))

    (:type "type" :supports ("rg") :language "scala"
           :regex "trait\\s*JJJ\\s*\\\(?"
           :tests ("trait test(object)"))

    (:type "type" :supports ("rg") :language "scala"
           :regex "object\\s*JJJ\\s*\\\(?"
           :tests ("object test(object)"))

    ;; solidity
    (:type "function" :supports ("rg") :language "solidity"
           :regex  "function\\s*JJJ\\s*\\\("
           :tests ("function test() internal" "function test (uint x, address y)" "function test() external"))

    (:type "modifier" :supports ("rg") :language "solidity"
           :regex  "modifier\\s*JJJ\\s*\\\("
           :tests ("modifier test()" "modifier test ()"))

    (:type "event" :supports ("rg") :language "solidity"
           :regex  "event\\s*JJJ\\s*\\\("
           :tests ("event test();" "event test (uint indexed x)" "event test(uint x, address y)"))

    (:type "error" :supports ("rg") :language "solidity"
           :regex  "error\\s*JJJ\\s*\\\("
           :tests ("error test();" "error test (uint x)" "error test(uint x, address y)"))

    (:type "contract" :supports ("rg") :language "solidity"
           :regex  "contract\\s*JJJ\\s*(is|\\\{)"
           :tests ("contract test{" "contract test {" "contract test is foo"))

    ;; R
    (:type "variable" :supports ("rg") :language "r"
           :regex "\\bJJJ\\s*=[^=><]" :tests ("test = 1234") :not ("if (test == 1234)"))

    (:type "function" :supports ("rg") :language "r"
           :regex "\\bJJJ\\s*<-\\s*function\\b"
           :tests ("test <- function" "test <- function(")
           :not   ("test <- functionX"))

    ;; perl
    (:type "function" :supports ("rg") :language "perl"
           :regex "sub\\s*JJJ\\s*(\\{|\\()"
           :tests ("sub test{" "sub test {" "sub test(" "sub test ("))

    (:type "variable" :supports ("rg") :language "perl"
           :regex "JJJ\\s*=\\s*"
           :tests ("$test = 1234"))

    ;; Tcl
    (:type "function" :supports ("rg") :language "tcl"
           :regex "proc\\s+JJJ\\s*\\{"
           :tests ("proc test{" "proc test {"))

    (:type "variable" :supports ("rg") :language "tcl"
           :regex "set\\s+JJJ"
           :tests ("set test 1234"))

    (:type "variable" :supports ("rg") :language "tcl"
           :regex "(variable|global)\\s+JJJ"
           :tests ("variable test" "global test"))

    ;; shell
    (:type "function" :supports ("rg") :language "shell"
           :regex "function\\s*JJJ\\s*"
           :tests ("function test{" "function test {" "function test () {")
           :not   ("function nottest {"))

    (:type "function" :supports ("rg") :language "shell"
           :regex "JJJ\\\(\\\)\\s*\\{"
           :tests ("test() {")
           :not ("testx() {"))

    (:type "variable" :supports ("rg") :language "shell"
           :regex "\\bJJJ\\s*=\\s*"
           :tests ("test = 1234") :not ("blahtest = 1234"))

    ;; php
    (:type "function" :supports ("rg") :language "php"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "function" :supports ("rg") :language "php"
           :regex "\\*\\s@method\\s+[^ \t]+\\s+JJJ\\("
           :tests ("/** @method string|false test($a)" " * @method bool test()"))

    (:type "variable" :supports ("rg") :language "php"
           :regex "(\\s|->|\\$|::)JJJ\\s*=\\s*"
           :tests ("$test = 1234" "$foo->test = 1234"))

    (:type "variable" :supports ("rg") :language "php"
           :regex "\\*\\s@property(-read|-write)?\\s+([^ \t]+\\s+)&?\\$JJJ(\\s+|$)"
           :tests ("/** @property string $test" "/** @property string $test description for $test property"  " * @property-read bool|bool $test" " * @property-write \\ArrayObject<string,resource[]> $test"))
    (:type "trait" :supports ("rg") :language "php"
           :regex "trait\\s*JJJ\\s*\\\{"
           :tests ("trait test{" "trait test {"))

    (:type "interface" :supports ("rg") :language "php"
           :regex "interface\\s*JJJ\\s*\\\{"
           :tests ("interface test{" "interface test {"))

    (:type "class" :supports ("rg") :language "php"
           :regex "class\\s*JJJ\\s*(extends|implements|\\\{)"
           :tests ("class test{" "class test {" "class test extends foo" "class test implements foo"))

    ;; dart
    (:type "function" :supports ("rg") :language "dart"
           :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
           :tests ("test(foo) {" "test (foo){" "test(foo){"))

    (:type "function" :supports ("rg") :language "dart"
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test(object) {" "class test{"))

    ;; faust
    (:type "function" :supports ("rg") :language "faust"
           :regex "\\bJJJ\(\\\(.+\\\)\)*\\s*="
           :tests ("test = osc + 0.5;" "test(freq) = osc(freq) + 0.5;"))

    ;; fennel
    (:type "variable" :supports ("rg") :language "fennel"
           :regex "\\((local|var)\\s+JJJ\\j"
           :tests ("(local test (foo)"
                   "(var test (foo)"))

    (:type "function" :supports ("rg") :language "fennel"
           :regex "\\(fn\\s+JJJ\\j"
           :tests ("(fn test [foo]")
           :not ("(fn test? [foo]"))

    (:type "function" :supports ("rg") :language "fennel"
           :regex "\\(macro\\s+JJJ\\j"
           :tests ("(macro test [foo]"))

    ;; fortran
    (:type "variable" :supports ("rg") :language "fortran"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if (test == 1234)"))

    (:type "function" :supports ("rg") :language "fortran"
           :regex "\\b(function|subroutine|FUNCTION|SUBROUTINE)\\s+JJJ\\b\\s*\\\("
           :tests ("function test (foo)" "integer function test(foo)"
                   "subroutine test (foo, bar)" "FUNCTION test (foo)"
                   "INTEGER FUNCTION test(foo)" "SUBROUTINE test (foo, bar)")
           :not ("end function test" "end subroutine test" "END FUNCTION test"
                 "END SUBROUTINE test"))

    (:type "function" :supports ("rg") :language "fortran"
           :regex "^\\s*(interface|INTERFACE)\\s+JJJ\\b"
           :tests ("interface test" "INTERFACE test")
           :not ("interface test2" "end interface test" "INTERFACE test2"
                 "END INTERFACE test"))

    (:type "type" :supports ("rg") :language "fortran"
           :regex "^\\s*(module|MODULE)\\s+JJJ\\s*"
           :tests ("module test" "MODULE test")
           :not ("end module test" "END MODULE test"))

    ;; go
    (:type "variable" :supports ("rg") :language "go"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test == 1234 {"))

    (:type "variable" :supports ("rg") :language "go"
           :regex "\\s*\\bJJJ\\s*:=\\s*" :tests ("test := 1234"))

    (:type "function" :supports ("rg") :language "go"
           :regex "func\\s+\\\([^\\\)]*\\\)\\s+JJJ\\s*\\\("
           :tests ("func (s *blah) test(filename string) string {"))

    (:type "function" :supports ("rg") :language "go"
           :regex "func\\s+JJJ\\s*\\\("
           :tests ("func test(url string) (string, error)"))

    (:type "type" :supports ("rg") :language "go"
           :regex "type\\s+JJJ\\s+struct\\s+\\\{"
           :tests ("type test struct {"))

    ;; javascript extended
    (:type "function" :supports ("rg") :language "javascript"
           :regex "(service|factory)\\\(['\"]JJJ['\"]" :tags ("angular")
           :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

    (:type "function" :supports ("rg") :language "javascript"
           :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>" :tags ("es6")
           :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

    (:type "function" :supports ("rg") :language "javascript"
           :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]" :tags ("es6")
           :tests ("test(foo) {" "test (foo){" "test(foo){")
           :not ("test = blah.then(function(){"))

    (:type "function" :supports ("rg") :language "javascript" :tags ("es6")
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test(object) {" "class test{"))

    (:type "function" :supports ("rg") :language "javascript" :tags ("es6")
           :regex "class\\s*JJJ\\s+extends"
           :tests ("class test extends Component{"))

    ;; javascript
    (:type "variable" :supports ("rg") :language "javascript"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234" "const test = props =>") :not ("if (test === 1234)"))

    (:type "variable" :supports ("rg") :language "javascript"
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
           :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
                 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

    (:type "function" :supports ("rg") :language "javascript"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "function" :supports ("rg") :language "javascript"
           :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
           :tests ("test: function()"))

    (:type "function" :supports ("rg") :language "javascript"
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    ;; hcl terraform
    (:type "block" :supports ("rg") :language "hcl"
           :regex "(variable|output|module)\\s*\"JJJ\"\\s*\\\{"
           :tests ("variable \"test\" {"
                   "output \"test\" {"
                   "module \"test\" {"))

    (:type "block" :supports ("rg") :language "hcl"
           :regex "(data|resource)\\s*\"\\w+\"\\s*\"JJJ\"\\s*\\\{"
           :tests ("data \"openstack_images_image_v2\" \"test\" {"
                   "resource \"google_compute_instance\" \"test\" {"))

    ;; typescript
    (:type "function" :supports ("rg") :language "typescript"
           :regex "(service|factory)\\\(['\"]JJJ['\"]" :tags ("angular")
           :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>"
           :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
           :tests ("test(foo) {" "test (foo){" "test(foo){")
           :not ("test = blah.then(function(){"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test{"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "class\\s*JJJ\\s+extends"
           :tests ("class test extends Component{"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
           :tests ("test: function()"))

    (:type "function" :supports ("rg") :language "typescript"
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    (:type "variable" :supports ("rg") :language "typescript"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234" "const test = props =>") :not ("if (test === 1234)"))

    (:type "variable" :supports ("rg") :language "typescript"
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
           :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
                 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

    ;; julia
    (:type "function" :supports ("rg") :language "julia"
           :regex "(@noinline|@inline)?\\s*function\\s*JJJ(\\{[^\\}]*\\})?\\("
           :tests ("function test()" "@inline function test()"
                   "function test{T}(h)"))

    (:type "function" :supports ("rg") :language "julia"
           :regex "(@noinline|@inline)?JJJ(\\{[^\\}]*\\})?\\([^\\)]*\\)\s*="
           :tests ("test(a)=1" "test(a,b)=1*8"
                   "@noinline test()=1" "test{T}(x)=x"))

    (:type "function" :supports ("rg") :language "julia"
           :regex "macro\\s*JJJ\\("
           :tests ("macro test(a)=1" " macro test(a,b)=1*8"))

    (:type "variable" :supports ("ag" "rg") :language "julia"
           :regex "const\\s+JJJ\\b"
           :tests ("const test = "))

    (:type "type" :supports ("ag" "rg") :language "julia"
           :regex "(mutable)?\\s*struct\\s*JJJ"
           :tests ("struct test"))

    (:type "type" :supports ("ag" "rg") :language "julia"
           :regex "(type|immutable|abstract)\\s*JJJ"
           :tests ("type test" "immutable test" "abstract test <:Testable" ))

    ;; haskell
    (:type "module" :supports ("ag") :language "haskell"
           :regex "^module\\s+JJJ\\s+"
           :tests ("module Test (exportA, exportB) where"))

                                        ; TODO Doesn't support any '=' in arguments. E.g. 'foo A{a = b,..} = bar'.
    (:type "top level function" :supports ("ag") :language "haskell"
           :regex "^\\bJJJ(?!(\\s+::))\\s+((.|\\s)*?)=\\s+"
           :tests ("test n = n * 2"
                   "test X{..} (Y a b c) \n bcd \n =\n x * y"
                   "test ab cd e@Datatype {..} (Another thing, inTheRow) = \n undefined"
                   "test = runRealBasedMode @ext @ctx identity identity"
                   "test unwrap wrap nr@Naoeu {..} (Action action, specSpecs) = \n    undefined")
           :not ("nottest n = n * 2"
                 "let testnot x y = x * y" "test $ y z" "let test a o = mda"
                 "test :: Sometype -> AnotherType aoeu kek = undefined"))

    (:type "type-like" :supports ("ag") :language "haskell"
           :regex "^\\s*((data(\\s+family)?)|(newtype)|(type(\\s+family)?))\\s+JJJ\\s+"
           :tests ("newtype Test a = Something { b :: Kek }"
                   "data Test a b = Somecase a | Othercase b"
                   "type family Test (x :: *) (xs :: [*]) :: Nat where"
                   "data family Test "
                   "type Test = TestAlias")
           :not ("newtype NotTest a = NotTest (Not a)"
                 "data TestNot b = Aoeu"))

                                        ; datatype contstuctor that doesn't match type definition.
    (:type "(data)type constructor 1" :supports ("ag") :language "haskell"
           :regex "(data|newtype)\\s{1,3}(?!JJJ\\s+)([^=]{1,40})=((\\s{0,3}JJJ\\s+)|([^=]{0,500}?((?<!(-- ))\\|\\s{0,3}JJJ\\s+)))"
           :tests ("data Something a = Test { b :: Kek }"
                   "data Mem a = TrueMem { b :: Kek } | Test (Mem Int) deriving Mda"
                   "newtype SafeTest a = Test (Kek a) deriving (YonedaEmbedding)")
           :not ("data Test = Test { b :: Kek }"))


    (:type "data/newtype record field" :supports ("ag") :language "haskell"
           :regex "(data|newtype)([^=]*)=[^=]*?({([^=}]*?)(\\bJJJ)\\s+::[^=}]+})"
           :tests ("data Mem = Mem { \n mda :: A \n  , test :: Kek \n , \n aoeu :: E \n }"
                   "data Mem = Mem { \n test :: A \n  , mda :: Kek \n , \n aoeu :: E \n }"
                   "data Mem = Mem { \n mda :: A \n  , aoeu :: Kek \n , \n test :: E \n }"
                   "data Mem = Mem { test :: Kek } deriving Mda"
                   "data Mem = Mem { \n test :: Kek \n } deriving Mda"
                   "newtype Mem = Mem { \n test :: Kek \n } deriving (Eq)"
                   "newtype Mem = Mem { -- | Some docs \n test :: Kek -- ^ More docs } deriving Eq"
                   "newtype Mem = Mem { test :: Kek } deriving (Eq,Monad)"
                   "newtype NewMem = OldMem { test :: [Tx] }"
                   "newtype BlockHeaderList ssc = BHL\n { test :: ([Aoeu a], [Ssss])\n    } deriving (Eq)")
           :not ("data Heh = Mda { sometest :: Kek, testsome :: Mem }"))

    (:type "typeclass" :supports ("ag") :language "haskell"
           :regex "^class\\s+(.+=>\\s*)?JJJ\\s+"
           :tests (
                   "class (Constr1 m, Constr 2) => Test (Kek a) where"
                   "class  Test  (Veryovka a)  where ")
           :not ("class Test2 (Kek a) where"
                 "class MakeTest (AoeuTest x y z) where"))

    ;; ocaml
    (:type "type" :supports ("ag" "rg") :language "ocaml"
           :regex "^\\s*(and|type)\\s+.*\\bJJJ\\b"
           :tests ("type test ="
                   "and test ="
                   "type 'a test ="
                   "type ('a, _, 'c) test"))

    (:type "variable" :supports ("ag" "rg") :language "ocaml"
           :regex "let\\s+JJJ\\b"
           :tests ("let test ="
                   "let test x y ="))

    (:type "variable" :supports ("ag" "rg") :language "ocaml"
           :regex "let\\s+rec\\s+JJJ\\b"
           :tests ("let rec test ="
                   "let rec  test x y ="))

    (:type "variable" :supports ("ag" "rg") :language "ocaml"
           :regex "\\s*val\\s*\\bJJJ\\b\\s*"
           :tests ("val test"))

    (:type "module" :supports ("ag" "rg") :language "ocaml"
           :regex "^\\s*module\\s*\\bJJJ\\b"
           :tests ("module test ="))

    (:type "module" :supports ("ag" "rg") :language "ocaml"
           :regex "^\\s*module\\s*type\\s*\\bJJJ\\b"
           :tests ("module type test ="))

    ;; lua
    (:type "variable" :supports ("rg") :language "lua"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test === 1234"))

    (:type "variable" :supports ("rg") :language "lua"
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah)" "function(blah, test)")
           :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah)" "function(blah, testLast)"
                 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah)" "function(blah, Lasttest)"))

    (:type "function" :supports ("rg") :language "lua"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "function" :supports ("rg") :language "lua"
           :regex "function\\s*.+[.:]JJJ\\s*\\\("
           :tests ("function MyClass.test()" "function MyClass.test ()"
                   "function MyClass:test()" "function MyClass:test ()"))

    (:type "function" :supports ("rg") :language "lua"
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    (:type "function" :supports ("rg") :language "lua"
           :regex "\\b.+\\.JJJ\\s*=\\s*function\\s*\\\("
           :tests ("MyClass.test = function()"))

    ;; rust
    (:type "variable" :supports ("rg") :language "rust"
           :regex "\\blet\\s+(\\\([^=\\n]*)?(mut\s+)?JJJ([^=\\n]*\\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+"
           :tests ("let test = 1234;"
                   "let test: u32 = 1234;"
                   "let test: Vec<u32> = Vec::new();"
                   "let mut test = 1234;"
                   "let mut test: Vec<u32> = Vec::new();"
                   "let (a, test, b) = (1, 2, 3);"
                   "let (a, mut test, mut b) = (1, 2, 3);"
                   "let (mut a, mut test): (u32, usize) = (1, 2);"))

    (:type "variable" :supports ("rg") :language "rust"
           :regex "\\bconst\\s+JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
           :tests ("const test: u32 = 1234;"))

    (:type "variable" :supports ("rg") :language "rust"
           :regex "\\bstatic\\s+(mut\\s+)?JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
           :tests ("static test: u32 = 1234;"
                   "static mut test: u32 = 1234;"))

    ;; variable in method signature
    (:type "variable" :supports ("rg") :language "rust"
           :regex "\\bfn\\s+.+\\s*\\\((.+,\\s+)?JJJ:\\s*[^=\\n]+\\s*(,\\s*.+)*\\\)"
           :tests ("fn abc(test: u32) -> u32 {"
                   "fn abc(x: u32, y: u32, test: Vec<u32>, z: Vec<Foo>)"
                   "fn abc(x: u32, y: u32, test: &mut Vec<u32>, z: Vec<Foo>)"))

    ;; "if let" and "while let" desugaring
    (:type "variable" :supports ("rg") :language "rust"
           :regex "(if|while)\\s+let\\s+([^=\\n]+)?(mut\\s+)?JJJ([^=\\n\\\(]+)?\\s*=\\s*[^=\\n]+"
           :tests ("if let Some(test) = abc() {"
                   "if let Some(mut test) = abc() {"
                   "if let Ok(test) = abc() {"
                   "if let Ok(mut test) = abc() {"
                   "if let Foo(mut test) = foo {"
                   "if let test = abc() {"
                   "if let Some(test) = abc()"
                   "if let Some((a, test, b)) = abc()"
                   "while let Some(test) = abc() {"
                   "while let Some(mut test) = abc() {"
                   "while let Ok(test) = abc() {"
                   "while let Ok(mut test) = abc() {")
           :not ("while let test(foo) = abc() {"))

    ;; structure fields
    (:type "variable" :supports ("rg") :language "rust"
           :regex "struct\\s+[^\\n{]+[{][^}]*(\\s*JJJ\\s*:\\s*[^\\n},]+)[^}]*}"
           :tests ("struct Foo { abc: u32, test: Vec<String>, b: PathBuf }"
                   "struct Foo<T>{test:Vec<T>}"
                   "struct FooBar<'a> { test: Vec<String> }")
           :not ("struct Foo { abc: u32, b: Vec<String> }"
                 "/// ... construct the equivalent ...\nfn abc() {\n"))

    ;; enum variants
    (:type "variable" :supports ("rg") :language "rust"
           :regex "enum\\s+[^\\n{]+\\s*[{][^}]*\\bJJJ\\b[^}]*}"
           :tests ("enum Foo { VariantA, test, VariantB(u32) }"
                   "enum Foo<T> { test(T) }"
                   "enum BadStyle{test}"
                   "enum Foo32 { Bar, testing, test(u8) }")
           :not ("enum Foo { testing }"))

    (:type "function" :supports ("rg") :language "rust"
           :regex "\\bfn\\s+JJJ\\s*\\\("
           :tests ("fn test(asdf: u32)" "fn test()" "pub fn test()"))

    (:type "function" :supports ("rg") :language "rust"
           :regex "\\bmacro_rules!\\s+JJJ"
           :tests ("macro_rules! test"))

    (:type "type" :supports ("rg") :language "rust"
           :regex "struct\\s+JJJ\\s*[{\\\(]?"
           :tests ("struct test(u32, u32)"
                   "struct test;"
                   "struct test { abc: u32, def: Vec<String> }"))

    (:type "type" :supports ("rg") :language "rust"
           :regex "trait\\s+JJJ\\s*[{]?"
           :tests ("trait test;" "trait test { fn abc() -> u32; }"))

    (:type "type" :supports ("rg") :language "rust"
           :regex "\\btype\\s+JJJ([^=\\n]+)?\\s*=[^=\\n]+;"
           :tests ("type test<T> = Rc<RefCell<T>>;"
                   "type test = Arc<RwLock<Vec<u32>>>;"))

    (:type "type" :supports ("rg") :language "rust"
           :regex "impl\\s+((\\w+::)*\\w+\\s+for\\s+)?(\\w+::)*JJJ\\s+[{]?"
           :tests ("impl test {"
                   "impl abc::test {"
                   "impl std::io::Read for test {"
                   "impl std::io::Read for abc::test {"))

    (:type "type" :supports ("rg") :language "rust"
           :regex "mod\\s+JJJ\\s*[{]?"
           :tests ("mod test;" "pub mod test {"))

    ;; elixir
    (:type "function" :supports ("rg") :language "elixir"
           :regex "\\bdef(p)?\\s+JJJ\\s*[ ,\\\(]"
           :tests ("def test do"
                   "def test, do:"
                   "def test() do"
                   "def test(), do:"
                   "def test(foo, bar) do"
                   "def test(foo, bar), do:"
                   "defp test do"
                   "defp test(), do:"))

    (:type "variable" :supports ("rg") :language "elixir"
           :regex "\\s*JJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234"))

    (:type "module" :supports ("rg") :language "elixir"
           :regex "defmodule\\s+(\\w+\\.)*JJJ\\s+"
           :tests ("defmodule test do"
                   "defmodule Foo.Bar.test do"))

    (:type "module" :supports ("rg") :language "elixir"
           :regex "defprotocol\\s+(\\w+\\.)*JJJ\\s+"
           :tests ("defprotocol test do"
                   "defprotocol Foo.Bar.test do"))

    ;; erlang
    (:type "function" :supports ("rg") :language "erlang"
           :regex "^JJJ\\b\\s*\\\("
           :tests ("test() ->"
                   "test()->"
                   "test(Foo) ->"
                   "test (Foo,Bar) ->"
                   "test(Foo, Bar)->"))

    (:type "variable" :supports ("rg") :language "erlang"
           :regex "\\s*JJJ\\s*=[^:=\\n]+"
           :tests ("test = 1234")
           :not ("if test =:= 1234"
                 "if test == 1234"))

    (:type "module" :supports ("rg") :language "erlang"
           :regex "^-module\\\(JJJ\\\)"
           :tests ("-module(test)."))

    ;; scss
    (:type "function" :supports ("rg") :language "scss"
           :regex "@mixin\\sJJJ\\b\\s*\\\("
           :tests ("@mixin test()"))

    (:type "function" :supports ("rg") :language "scss"
           :regex "@function\\sJJJ\\b\\s*\\\("
           :tests ("@function test()"))

    (:type "variable" :supports ("rg") :language "scss"
           :regex "JJJ\\s*:\\s*"
           :tests ("test  :"))

    ;; sml
    (:type "type" :supports ("rg") :language "sml"
           :regex "\\s*(data)?type\\s+.*\\bJJJ\\b"
           :tests ("datatype test ="
                   "datatype test="
                   "datatype 'a test ="
                   "type test ="
                   "type 'a test ="
                   "type 'a test"
                   "type test")
           :not ("datatypetest ="))

    (:type "variable" :supports ("rg") :language "sml"
           :regex "\\s*val\\s+\\bJJJ\\b"
           :tests ("val test ="
                   "val test="
                   "val test : bool"))

    (:type "function" :supports ("rg") :language "sml"
           :regex "\\s*fun\\s+\\bJJJ\\b.*\\s*="
           :tests ("fun test list ="
                   "fun test (STRING_NIL, a) ="
                   "fun test ((s1,s2): 'a queue) : 'a * 'a queue ="
                   "fun test (var : q) : int ="
                   "fun test f e xs ="))

    (:type "module" :supports ("rg") :language "sml"
           :regex "\\s*(structure|signature|functor)\\s+\\bJJJ\\b"
           :tests ("structure test ="
                   "structure test : MYTEST ="
                   "signature test ="
                   "functor test (T:TEST) ="
                   "functor test(T:TEST) ="))

    ;; sql
    (:type "function" :supports ("rg") :language "sql"
           :regex "(CREATE|create)\\s+(.+?\\s+)?(FUNCTION|function|PROCEDURE|procedure)\\s+JJJ\\s*\\\("
           :tests ("CREATE FUNCTION test(i INT) RETURNS INT"
                   "create or replace function test (int)"
                   "CREATE PROCEDURE test (OUT p INT)"
                   "create definer = 'test'@'localhost' procedure test()"))

    (:type "table" :supports ("rg") :language "sql"
           :regex "(CREATE|create)\\s+(.+?\\s+)?(TABLE|table)(\\s+(IF NOT EXISTS|if not exists))?\\s+JJJ\\b"
           :tests ("CREATE TABLE test ("
                   "create temporary table if not exists test"
                   "CREATE TABLE IF NOT EXISTS test ("
                   "create global temporary table test"))

    (:type "view" :supports ("rg") :language "sql"
           :regex "(CREATE|create)\\s+(.+?\\s+)?(VIEW|view)\\s+JJJ\\b"
           :tests ("CREATE VIEW test ("
                   "create sql security definer view test"
                   "CREATE OR REPLACE VIEW test AS foo"))

    (:type "type" :supports ("rg") :language "sql"
           :regex "(CREATE|create)\\s+(.+?\\s+)?(TYPE|type)\\s+JJJ\\b"
           :tests ("CREATE TYPE test"
                   "CREATE OR REPLACE TYPE test AS foo ("
                   "create type test as ("))

    ;; systemverilog
    (:type "type" :supports ("rg") :language "systemverilog"
           :regex "\\s*class\\s+\\bJJJ\\b"
           :tests ("virtual class test;" "class test;" "class test extends some_class")
           :not ("virtual class testing;" "class test2;" "class some_test" "class some_class extends test"))

    (:type "type" :supports ("rg") :language "systemverilog"
           :regex "\\s*task\\s+\\bJJJ\\b"
           :tests ("task test (" "task test(")
           :not ("task testing (" "task test2("))

    (:type "type" :supports ("rg") :language "systemverilog"
           :regex "\\s*\\bJJJ\\b\\s*="
           :tests ("assign test =" "assign test=" "int test =" "int test=")
           :not ("assign testing =" "assign test2="))

    (:type "function" :supports ("rg") :language "systemverilog"
           :regex "function\\s[^\\s]+\\s*\\bJJJ\\b"
           :tests ("function Matrix test ;" "function Matrix test;")
           :not ("function test blah"))

    ;; matches SV class handle declarations
    (:type "function" :supports ("rg") :language "systemverilog"
           :regex "^\\s*[^\\s]*\\s*[^\\s]+\\s+\\bJJJ\\b"
           :tests ("some_class_name test" "  another_class_name  test ;" "some_class test[];" "some_class #(1) test")
           :not ("test some_class_name" "class some_class extends test"))

    ;; vhdl
    (:type "type" :supports ("rg") :language "vhdl"
           :regex "\\s*type\\s+\\bJJJ\\b"
           :tests ("type test is" "type test  is")
           :not ("type testing is" "type test2  is"))

    (:type "type" :supports ("rg") :language "vhdl"
           :regex "\\s*constant\\s+\\bJJJ\\b"
           :tests ("constant test :" "constant test:")
           :not ("constant testing " "constant test2:"))

    (:type "function" :supports ("rg") :language "vhdl"
           :regex "function\\s*\"?JJJ\"?\\s*\\\("
           :tests ("function test(signal)" "function test (signal)" "function \"test\" (signal)")
           :not ("function testing(signal"))

    ;; latex
    (:type "command" :supports ("rg") :language "tex"
           :regex "\\\\.*newcommand\\\*?\\s*\\\{\\s*(\\\\)JJJ\\s*}"
           :tests ("\\newcommand{\\test}" "\\renewcommand{\\test}" "\\renewcommand*{\\test}" "\\newcommand*{\\test}" "\\renewcommand{ \\test }")
           :not("\\test"  "test"))

    (:type "command" :supports ("rg") :language "tex"
           :regex "\\\\.*newcommand\\\*?\\s*(\\\\)JJJ\\j"
           :tests ("\\newcommand\\test {}" "\\renewcommand\\test{}" "\\newcommand \\test")
           :not("\\test"  "test"))

    (:type "length" :supports ("rg") :language "tex"
           :regex "\\\\(s)etlength\\s*\\\{\\s*(\\\\)JJJ\\s*}"
           :tests ("\\setlength { \\test}" "\\setlength{\\test}" "\\setlength{\\test}{morecommands}" )
           :not("\\test"  "test"))

    (:type "counter" :supports ("rg") :language "tex"
           :regex "\\\\newcounter\\\{\\s*JJJ\\s*}"
           :tests ("\\newcounter{test}" )
           :not("\\test"  "test"))

    (:type "environment" :supports ("rg") :language "tex"
           :regex "\\\\.*newenvironment\\s*\\\{\\s*JJJ\\s*}"
           :tests ("\\newenvironment{test}" "\\newenvironment {test}{morecommands}" "\\lstnewenvironment{test}" "\\newenvironment {test}" )
           :not("\\test"  "test" ))

    ;; pascal (todo: var, type, const)
    (:type "function" :supports ("rg") :language "pascal"
           :regex "\\bfunction\\s+JJJ\\b"
           :tests ("  function test : "))

    (:type "function" :supports ("rg") :language "pascal"
           :regex "\\bprocedure\\s+JJJ\\b"
           :tests ("  procedure test ; "))

    ;; f#
    (:type "variable" :supports ("rg") :language "fsharp"
           :regex "let\\s+JJJ\\b.*\\\="
           :tests ("let test = 1234" "let test() = 1234" "let test abc def = 1234")
           :not ("let testnot = 1234" "let testnot() = 1234" "let testnot abc def = 1234"))

    (:type "interface" :supports ("rg") :language "fsharp"
           :regex "member(\\b.+\\.|\\s+)JJJ\\b.*\\\="
           :tests ("member test = 1234" "member this.test = 1234")
           :not ("member testnot = 1234" "member this.testnot = 1234"))

    (:type "type" :supports ("rg") :language "fsharp"
           :regex "type\\s+JJJ\\b.*\\\="
           :tests ("type test = 1234")
           :not ("type testnot = 1234"))

    ;; kotlin
    (:type "function" :supports ("rg") :language "kotlin"
           :regex "fun\\s*(<[^>]*>)?\\s*JJJ\\s*\\("
           :tests ("fun test()" "fun <T> test()"))
    (:type "variable" :supports ("rg") :language "kotlin"
           :regex "(val|var)\\s*JJJ\\b"
           :not ("val testval" "var testvar")
           :tests ("val test " "var test"))
    (:type "type" :supports ("rg") :language "kotlin"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test" "class test : SomeInterface" "interface test"))

    ;; zig
    (:type "function" :supports ("rg") :language "zig"
           :regex "fn\\s+JJJ\\b"
           :tests ("fn test() void {"
                   "fn test(a: i32) i32 {"
                   "pub fn test(a: i32) i32 {"
                   "export fn test(a: i32) i32 {"
                   "extern \"c\" fn test(a: i32) i32 {"
                   "inline fn test(a: i32) i32 {"))

    (:type "variable" :supports ("rg") :language "zig"
           :regex "(var|const)\\s+JJJ\\b"
           :tests ("const test: i32 = 3;"
                   "var test: i32 = 3;"
                   "pub const test: i32 = 3;"))

    ;; protobuf
    (:type "message" :supports ("rg") :language "protobuf"
           :regex "message\\s+JJJ\\s*\\\{"
           :tests ("message test{" "message test {"))

    (:type "enum" :supports ("rg") :language "protobuf"
           :regex "enum\\s+JJJ\\s*\\\{"
           :tests ("enum test{" "enum test {"))

    ;; apex (literally the same regexes as java)
    (:type "function" :supports ("ag" "rg") :language "apex"
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                   "public static MyType test()" "private virtual SomeType test(param)" "static int test()"
                   "private foo[] test()")
           :not ("test()" "testnot()" "blah = new test()" "foo bar = test()"))

    (:type "variable" :supports ("rg") :language "apex"
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

    (:type "type" :supports ("rg") :language "apex"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:" "public class test implements Something")
           :not ("class testnot:" "public class testnot implements Something")))


  "List of regex patttern templates organized by language and type to use for generating the grep command."
  :group 'dumber-jump
  :type
  '(repeat
    (plist
     :options ((:type string)
               (:supports string)
               (:language string)
               (:regex string)
               (:tests (repeat string))
               (:not (repeat string))))))

;; https://github.com/BurntSushi/ripgrep/blob/master/ignore/src/types.rs#L99

;; TODO: remove agtype
(defcustom dumber-jump-language-file-exts
  '((:language "elisp" :ext "el" :agtype "elisp" :rgtype "elisp")
    (:language "elisp" :ext "el.gz" :agtype "elisp" :rgtype "elisp")
    (:language "commonlisp" :ext "lisp" :agtype "lisp" :rgtype "lisp")
    (:language "commonlisp" :ext "lsp" :agtype "lisp" :rgtype "lisp")
    (:language "c++" :ext "c" :agtype "cc" :rgtype "c")
    (:language "c++" :ext "h" :agtype "cc" :rgtype "c")
    (:language "c++" :ext "C" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "H" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "tpp" :agtype "cpp" :rgtype nil)
    (:language "c++" :ext "cpp" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "hpp" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "cxx" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "hxx" :agtype "cpp" :rgtype nil)
    (:language "c++" :ext "cc" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "hh" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "c++" :agtype nil :rgtype nil)
    (:language "c++" :ext "h++" :agtype nil :rgtype nil)
    (:language "coq" :ext "v" :agtype nil :rgtype nil)
    (:language "ocaml" :ext "ml" :agtype "ocaml" :rgtype "ocaml")
    (:language "ocaml" :ext "mli" :agtype "ocaml" :rgtype "ocaml")
    (:language "ocaml" :ext "mll" :agtype "ocaml" :rgtype "ocaml")
    (:language "ocaml" :ext "mly" :agtype "ocaml" :rgtype "ocaml")
    ;; groovy is nil type because jenkinsfile is not in searcher type lists
    (:language "groovy" :ext "gradle" :agtype nil :rgtype nil)
    (:language "groovy" :ext "groovy" :agtype nil :rgtype nil)
    (:language "groovy" :ext "jenkinsfile" :agtype nil :rgtype nil)
    (:language "haskell" :ext "hs" :agtype "haskell" :rgtype "haskell")
    (:language "haskell" :ext "lhs" :agtype "haskell" :rgtype "haskell")
    (:language "objc" :ext "m" :agtype "objc" :rgtype "objc")
    (:language "csharp" :ext "cs" :agtype "csharp" :rgtype "csharp")
    (:language "java" :ext "java" :agtype "java" :rgtype "java")
    (:language "vala" :ext "vala" :agtype "vala" :rgtype "vala")
    (:language "vala" :ext "vapi" :agtype "vala" :rgtype "vala")
    (:language "julia" :ext "jl" :agtype "julia" :rgtype "julia")
    (:language "clojure" :ext "clj" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljc" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljs" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljx" :agtype "clojure" :rgtype "clojure")
    (:language "coffeescript" :ext "coffee" :agtype "coffee" :rgtype "coffeescript")
    (:language "faust" :ext "dsp" :agtype nil :rgtype nil)
    (:language "faust" :ext "lib" :agtype nil :rgtype nil)
    (:language "fennel" :ext "fnl" :agtype nil :rgtype nil)
    (:language "fortran" :ext "F" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f77" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f90" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f95" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "F77" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "F90" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "F95" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f03" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "for" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "ftn" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "fpp" :agtype "fortran" :rgtype "fortran")
    (:language "go" :ext "go" :agtype "go" :rgtype "go")
    (:language "javascript" :ext "js" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "jsx" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "vue" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "html" :agtype "html" :rgtype "html")
    (:language "javascript" :ext "css" :agtype "css" :rgtype "css")
    (:language "typescript" :ext "ts" :agtype "ts" :rgtype "ts")
    (:language "typescript" :ext "tsx" :agtype "ts" :rgtype "ts")
    (:language "typescript" :ext "vue" :agtype "ts" :rgtype "ts")
    (:language "dart" :ext "dart" :agtype nil :rgtype "dart")
    (:language "lua" :ext "lua" :agtype "lua" :rgtype "lua")
    ;; the extension "m" is also used by obj-c so must use matlab-mode
    ;; since obj-c will win by file extension, but here for searcher types
    (:language "matlab" :ext "m" :agtype "matlab" :rgtype "matlab")
    (:language "nim" :ext "nim" :agtype "nim" :rgtype "nim")
    (:language "nix" :ext "nix" :agtype "nix" :rgtype "nix")
    (:language "org" :ext "org" :agtype nil :rgtype "org")
    (:language "perl" :ext "pl" :agtype "perl" :rgtype "perl")
    (:language "perl" :ext "pm" :agtype "perl" :rgtype "perl")
    (:language "perl" :ext "pm6" :agtype "perl" :rgtype nil)
    (:language "perl" :ext "perl" :agtype nil :rgtype "perl")
    (:language "perl" :ext "plh" :agtype nil :rgtype "perl")
    (:language "perl" :ext "plx" :agtype nil :rgtype "perl")
    (:language "perl" :ext "pod" :agtype "perl" :rgtype "pod")
    (:language "perl" :ext "t" :agtype "perl" :rgtype nil)
    (:language "php" :ext "php" :agtype "php" :rgtype "php")
    (:language "php" :ext "php3" :agtype "php" :rgtype "php")
    (:language "php" :ext "php4" :agtype "php" :rgtype "php")
    (:language "php" :ext "php5" :agtype "php" :rgtype "php")
    (:language "php" :ext "phtml" :agtype "php" :rgtype "php")
    (:language "php" :ext "inc" :agtype "php" :rgtype nil)
    (:language "python" :ext "py" :agtype "python" :rgtype "py")
    (:language "r" :ext "R" :agtype "r" :rgtype "r")
    (:language "r" :ext "r" :agtype "r" :rgtype "r")
    (:language "r" :ext "Rmd" :agtype "r" :rgtype "r")
    (:language "r" :ext "Rnw" :agtype "r" :rgtype "r")
    (:language "r" :ext "Rtex" :agtype "r" :rgtype nil)
    (:language "r" :ext "Rrst" :agtype "r" :rgtype nil)
    (:language "racket" :ext "rkt" :agtype "racket" :rgtype "lisp")
    (:language "crystal" :ext "cr" :agtype "crystal" :rgtype "crystal")
    (:language "crystal" :ext "ecr" :agtype "crystal" :rgtype nil)
    (:language "ruby" :ext "rb" :agtype "ruby" :rgtype "ruby")
    (:language "ruby" :ext "erb" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "haml" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "rake" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "slim" :agtype "ruby" :rgtype nil)
    (:language "rust" :ext "rs" :agtype "rust" :rgtype "rust")
    (:language "zig" :ext "zig" :agtype nil :rgtype "zig")
    (:language "scad" :ext "scad" :agtype nil :rgtype nil)
    (:language "scala" :ext "scala" :agtype "scala" :rgtype "scala")
    (:language "scheme" :ext "scm" :agtype "scheme" :rgtype "lisp")
    (:language "scheme" :ext "ss" :agtype "scheme" :rgtype "lisp")
    (:language "scheme" :ext "sld" :agtype "scheme" :rgtype "lisp")
    (:language "janet" :ext "janet" :agtype "janet" :rgtype "lisp")
    (:language "shell" :ext "sh" :agtype nil :rgtype nil)
    (:language "shell" :ext "bash" :agtype nil :rgtype nil)
    (:language "shell" :ext "csh" :agtype nil :rgtype nil)
    (:language "shell" :ext "ksh" :agtype nil :rgtype nil)
    (:language "shell" :ext "tcsh" :agtype nil :rgtype nil)
    (:language "sml" :ext "sml" :agtype "sml" :rgtype "sml")
    (:language "solidity" :ext "sol" :agtype nil :rgtype nil)
    (:language "sql" :ext "sql" :agtype "sql" :rgtype "sql")
    (:language "swift" :ext "swift" :agtype nil :rgtype "swift")
    (:language "tex" :ext "tex" :agtype "tex" :rgtype "tex")
    (:language "elixir" :ext "ex" :agtype "elixir" :rgtype "elixir")
    (:language "elixir" :ext "exs" :agtype "elixir" :rgtype "elixir")
    (:language "elixir" :ext "eex" :agtype "elixir" :rgtype "elixir")
    (:language "erlang" :ext "erl" :agtype "erlang" :rgtype "erlang")
    (:language "systemverilog" :ext "sv" :agtype "verilog" :rgtype "verilog")
    (:language "systemverilog" :ext "svh" :agtype "verilog" :rgtype "verilog")
    (:language "vhdl" :ext "vhd" :agtype "vhdl" :rgtype "vhdl")
    (:language "vhdl" :ext "vhdl" :agtype "vhdl" :rgtype "vhdl")
    (:language "scss" :ext "scss" :agtype "css" :rgtype "css")
    (:language "pascal" :ext "pas" :agtype "delphi" :rgtype nil)
    (:language "pascal" :ext "dpr" :agtype "delphi" :rgtype nil)
    (:language "pascal" :ext "int" :agtype "delphi" :rgtype nil)
    (:language "pascal" :ext "dfm" :agtype "delphi" :rgtype nil)
    (:language "fsharp" :ext "fs" :agtype "fsharp" :rgtype nil)
    (:language "fsharp" :ext "fsi" :agtype "fsharp" :rgtype nil)
    (:language "fsharp" :ext "fsx" :agtype "fsharp" :rgtype nil)
    (:language "kotlin" :ext "kt" :agtype "kotlin" :rgtype "kotlin")
    (:language "kotlin" :ext "kts" :agtype "kotlin" :rgtype "kotlin")
    (:language "protobuf" :ext "proto" :agtype "proto" :rgtype "protobuf")
    (:language "hcl" :ext "tf" :agtype "terraform" :rgtype "tf")
    (:language "hcl" :ext "tfvars" :agtype "terraform" :rgtype nil)
    (:language "apex" :ext "cls" :agtype nil :rgtype nil)
    (:language "apex" :ext "trigger" :agtype nil :rgtype nil))

  "Mapping of programming language(s) to file extensions."
  :group 'dumber-jump
  :type
  '(repeat
    (plist
     :options ((:language (string :tag "Language"))
               (:ext (string :tag "Extension"))
               (:agtype (string :tag "Ag type"))
               (:rgtype (string :tag "Ripgrep type"))))))

(defcustom dumber-jump-language-contexts
  '((:language "javascript" :type "function" :right "^(" :left nil)
    (:language "javascript" :type "variable" :right nil :left "($")
    (:language "javascript" :type "variable" :right "^)" :left "($")
    (:language "javascript" :type "variable" :right "^\\." :left nil)
    (:language "javascript" :type "variable" :right "^;" :left nil)
    (:language "typescript" :type "function" :right "^(" :left nil)
    (:language "perl" :type "function" :right "^(" :left nil)
    (:language "tcl" :type "function" :left "\\[$" :right nil)
    (:language "tcl" :type "function" :left "^\s*$" :right nil)
    (:language "tcl" :type "variable" :left "\\$$" :right nil)
    (:language "php" :type "function" :right "^(" :left nil)
    (:language "php" :type "class" :right nil :left "new\s+")
    (:language "elisp" :type "function" :right nil :left "($")
    (:language "elisp" :type "variable" :right "^)" :left nil)
    (:language "scheme" :type "function" :right nil :left "($")
    (:language "scheme" :type "variable" :right "^)" :left nil))

  "List of under points contexts for each language.
This helps limit the number of regular expressions we use
if we know that if there's a `(' immediately to the right of
a symbol then it's probably a function call"
  :group 'dumber-jump
  :type
  '(repeat
    (plist
     :options ((:language (string :tag "Language"))
               (:type (choice (const "function")
                              (const "variable")))
               (:left (choice (const :tag "Anything" nil)
                              (string :tag "Regular expression")))
               (:right (choice (const :tag "Anything" nil)
                               (string :tag "Regular expression")))))))

(defcustom dumber-jump-project-denoters
  '(".dumberjump" ".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "Rakefile" "PkgInfo" "-pkg.el" "_FOSSIL_")
  "Files and directories that signify a directory is a project root."
  :group 'dumber-jump
  :type '(repeat (string  :tag "Name")))

;; TODO: remove this functionality, this is absurd
(defcustom dumber-jump-default-project "~"
  "The default project to search within if a project root is not found."
  :group 'dumber-jump
  :type 'string)

(defcustom dumber-jump-project nil
  "The project to search within if normal denoters will not work.  This should only be needed in the rarest of cases."
  :group 'dumber-jump
  :type 'string)

(defcustom dumber-jump-aggressive
  nil
  "If `t` jump aggressively with the possibility of a false positive.
If `nil` always show list of more than 1 match."
  :group 'dumber-jump
  :type 'boolean)

(defcustom dumber-jump-debug
  nil
  "If `t` will print helpful debug information."
  :group 'dumber-jump
  :type 'boolean)

(defvar dumber-jump--rg-installed? 'unset)
(defun dumber-jump-rg-installed? ()
  "Return t if rg is installed."
  (if (eq dumber-jump--rg-installed? 'unset)
      (setq dumber-jump--rg-installed?
            (let ((result (s-match "ripgrep \\([0-9]+\\)\\.\\([0-9]+\\).*"
                                   (shell-command-to-string (concat dumber-jump-rg-cmd " --version")))))
              (when (equal (length result) 3)
                (let ((major (string-to-number (nth 1 result)))
                      (minor (string-to-number (nth 2 result))))
                  (or
                   (and (= major 0) (>= minor 10))
                   (>= major 1))))))
    dumber-jump--rg-installed?))

(defun dumber-jump-message (str &rest args)
  "Log message STR with ARGS to the *Messages* buffer if not using `dumber-jump-quiet'."
  (when (not dumber-jump-quiet)
    (apply #'message str args))
  nil)

(defmacro dumber-jump-debug-message (&rest exprs)
  "Generate a debug message to print all expressions EXPRS."
  (declare (indent defun))
  (let ((i 5) frames frame)
    ;; based on https://emacs.stackexchange.com/a/2312
    (while (setq frame (backtrace-frame i))
      (push frame frames)
      (cl-incf i))
    ;; this is a macro-expanded version of the code in the stackexchange
    ;; code from above. This version should work on emacs-24.3, since it
    ;; doesn't depend on thread-last.
    (let* ((frame (cl-find-if
                   (lambda (frame)
                     (ignore-errors
                       (and (car frame)
                            (eq (caaddr frame)
                                'defalias))))
                   (reverse frames)))
           (func (cl-cadadr (cl-caddr frame)))
           (defun-name (symbol-name func)))
      (with-temp-buffer
        (insert "DUMBER JUMP DEBUG `")
        (insert defun-name)
        (insert "` START\n----\n\n")
        (dolist (expr exprs)
          (insert (prin1-to-string expr) ":\n\t%s\n\n"))
        (insert "\n-----\nDUMBER JUMP DEBUG `")
        (insert defun-name)
        (insert "` END\n-----")
        `(when dumber-jump-debug
           (dumber-jump-message
            ,(buffer-string)
            ,@exprs))))))

(defun dumber-jump-get-point-context (line func cur-pos)
  "Get the LINE context to the left and right of FUNC using CUR-POS as hint."
  (let ((loc (or (cl-search func line :start2 cur-pos) 0)))
    (list :left (substring line 0 loc)
          :right (substring line (+ loc (length func))))))

(defun dumber-jump-get-project-root (filepath)
  "Keep looking at the parent dir of FILEPATH until a denoter file/dir is found."
  (s-chop-suffix
   "/"
   (expand-file-name
    (or
     dumber-jump-project
     (locate-dominating-file filepath #'dumber-jump-get-config)
     dumber-jump-default-project))))

(defun dumber-jump-get-config (dir)
  "If a project denoter is in DIR then return it, otherwise nil.
However, if DIR contains a `.dumbjumpignore' it returns nil
to keep looking for another root."
  (if (file-exists-p (expand-file-name ".dumbjumpignore" dir))
      nil
    (car (--filter
          (file-exists-p (expand-file-name it dir))
          dumber-jump-project-denoters))))

(defun dumber-jump-get-language (file)
  "Get language from FILE extension and then fallback to using `major-mode' name."
  (let* ((languages (-distinct
                     (--map (plist-get it :language)
                            dumber-jump-find-rules)))
         (language (or (dumber-jump-get-language-from-mode)
                       (dumber-jump-get-language-by-filename file)
                       (dumber-jump-get-mode-base-name))))
    (if (member language languages)
        language
      (format ".%s file" (or (file-name-extension file) "")))))

(defun dumber-jump-get-mode-base-name ()
  "Get the base name of the mode."
  (s-replace "-mode" "" (symbol-name major-mode)))

(defun dumber-jump-get-language-from-mode ()
  "Extract the language from the `major-mode' name.  Currently just everything before `-mode'."
  (let* ((lookup '(sh "shell" cperl "perl" matlab "matlab" octave "matlab"))
         (m (dumber-jump-get-mode-base-name))
         (result (plist-get lookup (intern m))))
    result))

(defun dumber-jump-get-language-by-filename (file)
  "Get the programming language from the FILE."
  (let* ((filename (if (s-ends-with? ".gz" file)
                       (file-name-sans-extension file)
                     file))
         (result (--filter
                  (s-ends-with? (concat "." (plist-get it :ext)) filename)
                  dumber-jump-language-file-exts)))
    (when result
      (plist-get (car result) :language))))

(defun dumber-jump-issue-result (issue)
  "Return a result property list with the ISSUE set as :issue property symbol."
  `(:results nil :lang nil :symbol nil :ctx-type nil :file nil :root nil :issue ,(intern issue)))

(defun dumber-jump-get-results (&optional prompt)
  "Run dumber-jump-fetch-results if searcher installed, buffer is saved, and there's a symbol under point."
  (cond
   ((not (dumber-jump-rg-installed?))
    (dumber-jump-issue-result "nogrep"))
   ((or (string= (buffer-name) "*shell*") ; TODO: seriously?!?
        (string= (buffer-name) "*eshell*"))
    (dumber-jump-fetch-shell-results prompt))
   ((and (not prompt) (not (region-active-p)) (not (thing-at-point 'symbol)))
    (dumber-jump-issue-result "nosymbol"))
   (t
    (dumber-jump-fetch-file-results prompt))))

(defun dumber-jump-fetch-shell-results (&optional prompt)
  (let* ((cur-file (buffer-name))
         (proj-root (dumber-jump-get-project-root default-directory))
         (proj-config (dumber-jump-get-config proj-root))
         (config (when (s-ends-with? ".dumbjump" proj-config)
                   (dumber-jump-read-config proj-root proj-config)))
         (lang (or (plist-get config :language)
                   (car (dumber-jump-get-lang-by-shell-contents (buffer-name))))))
    (dumber-jump-fetch-results cur-file proj-root lang config prompt)))

(defun dumber-jump-fetch-file-results (&optional prompt)
  (let* ((cur-file (or (buffer-file-name) ""))
         (proj-root (dumber-jump-get-project-root cur-file))
         (proj-config (dumber-jump-get-config proj-root))
         (config (when (s-ends-with? ".dumbjump" proj-config)
                   (dumber-jump-read-config proj-root proj-config)))
         (lang (or (plist-get config :language)
                   (dumber-jump-get-language cur-file))))
    (dumber-jump-fetch-results cur-file proj-root lang config prompt)))

(defun dumber-jump-process-symbol-by-lang (lang look-for)
  "Process LANG's LOOK-FOR.  For instance, clojure needs namespace part removed."
  (cond
   ((and (string= lang "clojure") (s-contains? "/" look-for))
    (nth 1 (s-split "/" look-for)))
   ((and (string= lang "fennel") (s-contains? "." look-for))
    (-last-item (s-split "\\." look-for)))
   ((and (string= lang "ruby") (s-contains? "::" look-for))
    (-last-item (s-split "::" look-for)))
   ((and (or (string= lang "ruby") (string= lang "crystal")) (s-starts-with? ":" look-for))
    (s-chop-prefix ":" look-for))
   ((and (string= lang "systemverilog") (s-starts-with? "`" look-for))
    (s-chop-prefix "`" look-for))
   (t
    look-for)))

(defun dumber-jump-get-point-line ()    ; TODO: remove
  "Get line at point."
  (thing-at-point 'line t))

(defun dumber-jump-get-point-symbol ()  ; TODO: remove?
  "Get symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun dumber-jump--get-symbol-start ()
  "Get the start of symbol at point."
  (- (if (region-active-p)
         (region-beginning)
       (car (bounds-of-thing-at-point 'symbol)))
     (line-beginning-position)))

(defun dumber-jump-get-lang-by-shell-contents (buffer)
  "Return languages in BUFFER by checking if file extension is mentioned."
  (let* ((buffer-contents (with-current-buffer buffer
                            (buffer-string)))

         (found (--filter (s-match (concat "\\." (plist-get it :ext) "\\b") buffer-contents)
                          dumber-jump-language-file-exts)))
    (--map (plist-get it :language) found)))

(defun dumber-jump-fetch-results (cur-file proj-root lang _config &optional prompt)
  "Return a list of results based on current file context and calling rg.
CUR-FILE is the path of the current buffer.
PROJ-ROOT is that file's root project directory.
LANG is a string programming language with CONFIG a property list
of project configuration."
  (let* ((cur-line-num (line-number-at-pos))
         (proj-config (dumber-jump-get-config proj-root))
         (config (when (s-ends-with? ".dumbjump" proj-config)
                   (dumber-jump-read-config proj-root proj-config)))
         (found-symbol (or prompt (dumber-jump-get-point-symbol)))
         (look-for (dumber-jump-process-symbol-by-lang lang found-symbol))
         (pt-ctx (if prompt
                     (get-text-property 0 :dumber-jump-ctx prompt)
                   (dumber-jump-get-point-context
                    (dumber-jump-get-point-line)
                    look-for
                    (dumber-jump--get-symbol-start))))
         (ctx-type
          (dumber-jump-get-ctx-type-by-language lang pt-ctx))

         (parse-fn 'dumber-jump-parse-rg-response)      ; TODO: remove
         (generate-fn 'dumber-jump-generate-rg-command) ; TODO: remove
         (searcher 'rg)                                 ; TODO: remove

         (regexes (dumber-jump-get-contextual-regexes lang ctx-type searcher))

         (exclude-paths (when config (plist-get config :exclude)))
         (include-paths (when config (plist-get config :include)))
                                        ; we will search proj root and all include paths
         (search-paths (-distinct (-concat (list proj-root) include-paths)))
                                        ; run command for all
         (raw-results (--mapcat
                       ;; TODO: should only pass exclude paths to actual project root
                       (dumber-jump-run-command look-for it regexes lang exclude-paths cur-file
                                                cur-line-num parse-fn generate-fn)
                       search-paths))

         (results (delete-dups (--map (plist-put it :target look-for) raw-results))))

    `(:results ,results :lang ,(if (null lang) "" lang) :symbol ,look-for :ctx-type ,(if (null ctx-type) "" ctx-type) :file ,cur-file :root ,proj-root)))

(defcustom dumber-jump-language-comments
  '((:comment "//" :language "c++")
    (:comment ";" :language "elisp")
    (:comment ";" :language "commonlisp")
    (:comment "//" :language "javascript")
    (:comment "//" :language "typescript")
    (:comment "//" :language "dart")
    (:comment "--" :language "haskell")
    (:comment "--" :language "lua")
    (:comment "//" :language "rust")
    (:comment "#"  :language "julia")
    (:comment "//" :language "objc")
    (:comment "//" :language "csharp")
    (:comment "//" :language "java")
    (:comment ";" :language "clojure")
    (:comment "#" :language "coffeescript")
    (:comment "//" :language "faust")
    (:comment ";" :language "fennel")
    (:comment "!" :language "fortran")
    (:comment "//" :language "go")
    (:comment "//" :language "zig")
    (:comment "#" :language "perl")
    (:comment "#" :language "tcl")
    (:comment "//" :language "php")
    (:comment "#" :language "python")
    (:comment "%" :language "matlab")
    (:comment "#" :language "r")
    (:comment ";" :language "racket")
    (:comment "#" :language "ruby")
    (:comment "#" :language "crystal")
    (:comment "#" :language "nim")
    (:comment "#" :language "nix")
    (:comment "//" :language "scala")
    (:comment ";" :language "scheme")
    (:comment "#" :language "janet")
    (:comment "#" :language "shell")
    (:comment "//" :language "solidity")
    (:comment "//" :language "swift")
    (:comment "#" :language "elixir")
    (:comment "%" :language "erlang")
    (:comment "%" :language "tex")
    (:comment "//" :language "systemverilog")
    (:comment "--" :language "vhdl")
    (:comment "//" :language "scss")
    (:comment "//" :language "pascal")
    (:comment "//" :language "protobuf")
    (:comment "#" :language "hcl")
    (:comment "//" :language "apex"))
  "List of one-line comments organized by language."
  :group 'dumber-jump
  :type
  '(repeat
    (plist
     :options ((:comment string)
               (:language string)))))

(defun dumber-jump-get-comment-by-language (lang)
  "Yields the one-line comment for the given LANG."
  (let* ((entries (-distinct
                   (--filter (string= (plist-get it :language) lang)
                             dumber-jump-language-comments))))
    (if (= 1 (length entries))
        (plist-get (car entries) :comment)
      nil)))

(defun dumber-jump-filter-no-start-comments (results lang)
  "Filter out RESULTS with a :context that start with a comment given the LANG of the current file."
  (let ((comment (dumber-jump-get-comment-by-language lang)))
    (if comment
        (-concat
         (--filter (not (s-starts-with? comment (s-trim (plist-get it :context)))) results))
      results)))

(defun dumber-jump-process-results
    (results cur-file proj-root ctx-type _look-for _use-tooltip prefer-external)
  "Process (filter, sort, ...) the searchers results.
RESULTS is a list of property lists with the searcher's results.
CUR-FILE is the current file within PROJ-ROOT.
CTX-TYPE is a string of the current context.
LOOK-FOR is the symbol we're jumping for.
USE-TOOLTIP shows a preview instead of jumping.
PREFER-EXTERNAL will sort current file last.

Figure which of the RESULTS to jump to. Favoring the CUR-FILE"
  (let* ((lang (dumber-jump-get-language-by-filename cur-file))
         (match-sorted (-sort (lambda (x y) (< (plist-get x :diff) (plist-get y :diff))) results))
         (match-no-comments (dumber-jump-filter-no-start-comments match-sorted lang))

         ;; Find the relative current file path by the project root. In some cases the results will
         ;; not be absolute but relative and the "current file" filters must match in both
         ;; cases. Also works when current file is in an arbitrary sub folder.
         (rel-cur-file
          (cond ((and (s-starts-with? proj-root cur-file)
                      (s-starts-with? default-directory cur-file))
                 (substring cur-file (length default-directory) (length cur-file)))

                ((and (s-starts-with? proj-root cur-file)
                      (not (s-starts-with? default-directory cur-file)))
                 (substring cur-file (1+ (length proj-root)) (length cur-file)))

                (t
                 cur-file)))

         ;; Moves current file results to the front of the list, unless PREFER-EXTERNAL then put
         ;; them last.
         (match-cur-file-front
          (if (not prefer-external)
              (-concat
               (--filter (and (> (plist-get it :diff) 0)
                              (or (string= (plist-get it :path) cur-file)
                                  (string= (plist-get it :path) rel-cur-file)))
                         match-no-comments)
               (--filter (and (<= (plist-get it :diff) 0)
                              (or (string= (plist-get it :path) cur-file)
                                  (string= (plist-get it :path) rel-cur-file)))
                         match-no-comments)

               ;; Sort non-current files by path length so the nearest file is more likely to be
               ;; sorted higher to the top. Also sorts by line number for sanity.
               (-sort (lambda (x y)
                        (and (< (plist-get x :line) (plist-get y :line))
                             (< (length (plist-get x :path)) (length (plist-get y :path)))))
                      (--filter (not (or (string= (plist-get it :path) cur-file)
                                         (string= (plist-get it :path) rel-cur-file)))
                                match-no-comments)))
            (-concat
             (-sort (lambda (x y)
                      (and (< (plist-get x :line) (plist-get y :line))
                           (< (length (plist-get x :path)) (length (plist-get y :path)))))
                    (--filter (not (or (string= (plist-get it :path) cur-file)
                                       (string= (plist-get it :path) rel-cur-file)))
                              match-no-comments))
             (--filter (or (string= (plist-get it :path) cur-file)
                           (string= (plist-get it :path) rel-cur-file))
                       match-no-comments))))

         (matches
          (if (not prefer-external)
              (-distinct
               (append (dumber-jump-current-file-results cur-file match-cur-file-front)
                       (dumber-jump-current-file-results rel-cur-file match-cur-file-front)))
            match-cur-file-front))

         (var-to-jump (car matches))
         ;; TODO: handle if ctx-type is null but ALL results are variable

         ;; When non-aggressive it should only jump when there is only one match, regardless of
         ;; context.
         (do-var-jump
          (and (or dumber-jump-aggressive
                   (= (length match-cur-file-front) 1))
               (or (= (length matches) 1)
                   (string= ctx-type "variable")
                   (string= ctx-type ""))
               var-to-jump)))

    (list :results results
          :do-var-jump do-var-jump
          :var-to-jump var-to-jump
          :match-cur-file-front match-cur-file-front)))

(defun dumber-jump-read-config (root config-file)
  "Load and return options (exclusions, inclusions, etc).
Ffrom the ROOT project CONFIG-FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name config-file root))
    (let ((local-root (if (file-remote-p root)
                          (tramp-file-name-localname
                           (tramp-dissect-file-name root))
                        root))
          include exclude lang)
      (while (not (eobp))
        (cond ((looking-at "^language \\\(.+\\\)")
               (setq lang (match-string 1)))
              ((looking-at "^\\+\\(.+\\)")
               (push (expand-file-name (match-string 1) local-root)
                     include))
              ((looking-at "^-/?\\(.+\\)")
               (push (expand-file-name (match-string 1) local-root)
                     exclude)))
        (forward-line))
      (list :exclude (nreverse exclude)
            :include (nreverse include)
            :language lang))))

(defun dumber-jump-current-file-results (path results)
  "Return the PATH's RESULTS."
  (let ((matched (--filter (string= path (plist-get it :path)) results)))
    matched))

(defun dumber-jump-shell-command-switch ()
  "Yields the shell command switch to use for the current `shell-file-name' in order to not load the shell profile/RC for speeding up things."
  (let ((base-name (downcase (file-name-base shell-file-name))))
    (cond
     ((or (string-equal "zsh" base-name)
          (string-equal "csh" base-name)
          (string-equal "tcsh" base-name))
      "-icf")

     ((string-equal "bash" base-name)
      "-c")

     (t
      shell-command-switch))))

;; TODO: rename dumber-jump-run-definition-command
(defun dumber-jump-run-command
    (look-for proj regexes lang exclude-args cur-file line-num parse-fn generate-fn)
  "Run the command based on the needle LOOK-FOR in the directory TOSEARCH."
  (let* ((proj-root (if (file-remote-p proj)
                        (directory-file-name
                         (tramp-file-name-localname (tramp-dissect-file-name proj)))
                      proj))
         (cmd (funcall generate-fn look-for cur-file proj-root regexes lang exclude-args))
         (shell-command-switch (dumber-jump-shell-command-switch))
         (rawresults (shell-command-to-string cmd)))

    (dumber-jump-debug-message cmd rawresults)
    (when (and (s-blank? rawresults) dumber-jump-fallback-search)
      (setq regexes (list dumber-jump-fallback-regex))
      (setq cmd (funcall generate-fn look-for cur-file proj-root regexes lang exclude-args))
      (setq rawresults (shell-command-to-string cmd))
      (dumber-jump-debug-message cmd rawresults))
    (unless (s-blank? cmd)
      (let ((results (funcall parse-fn rawresults cur-file line-num)))
        (--filter (s-contains? look-for (plist-get it :context)) results)))))

;; TODO: extend to support column?
(defun dumber-jump-parse-response-line (resp-line cur-file)
  "Parse a search program's single RESP-LINE for CUR-FILE into a list of (path line context)."
  (let* ((parts (--remove (string= it "")
                          (s-split "\\(?:^\\|:\\)[0-9]+:"  resp-line)))
         (line-num-raw (s-match "\\(?:^\\|:\\)\\([0-9]+\\):" resp-line)))

    (cond
     ;; fixes rare bug where context is blank  but file is defined "/somepath/file.txt:14:"
     ;; OR: (and (= (length parts) 1) (file-name-exists (nth 0 parts)))
     ((s-match ":[0-9]+:$" resp-line)
      nil)
     ((and parts line-num-raw)
      (if (= (length parts) 2)
          (list (let ((path (expand-file-name (nth 0 parts))))
                  (if (file-name-absolute-p (nth 0 parts))
                      path
                    (file-relative-name path)))
                (nth 1 line-num-raw) (nth 1 parts))
                                        ; this case is when they are searching a particular file...
        (list (let ((path (expand-file-name cur-file)))
                (if (file-name-absolute-p cur-file)
                    path
                  (file-relative-name path)))
              (nth 1 line-num-raw) (nth 0 parts)))))))

(defun dumber-jump-parse-response-lines (parsed cur-file cur-line-num)
  "Turn PARSED response lines into a list of property lists.  Using CUR-FILE and CUR-LINE-NUM to exclude jump origin."
  (let* ((records (--mapcat (when it
                              (let* ((line-num (string-to-number (nth 1 it)))
                                     (diff (- cur-line-num line-num)))
                                (list `(:path ,(nth 0 it) :line ,line-num :context ,(nth 2 it) :diff ,diff))))
                            parsed))
         (results (-non-nil records)))
    (--filter
     (not (and
           (string= (plist-get it :path) cur-file)
           (= (plist-get it :line) cur-line-num)))
     results)))

(defun dumber-jump-parse-rg-response (resp cur-file cur-line-num)
  "Takes a rg response RESP and parses into a list of plists."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumber-jump-parse-response-line it cur-file) resp-lines)))
    (dumber-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumber-jump-re-match (re s)
  "Does regular expression RE match string S. If RE is nil return nil."
  (when (and re s)
    (s-match re s)))

(defun dumber-jump-get-ctx-type-by-language (lang pt-ctx)
  "Detect the type of context by the language LANG and its context PT-CTX."
  (let* ((contexts (--filter (string= (plist-get it ':language) lang) dumber-jump-language-contexts))
         (usable-ctxs
          (when (> (length contexts) 0)
            (--filter (and (or (null (plist-get it :left))
                               (dumber-jump-re-match (plist-get it :left)
                                                     (plist-get pt-ctx :left)))
                           (or (null (plist-get it :right))
                               (dumber-jump-re-match (plist-get it :right)
                                                     (plist-get pt-ctx :right))))
                      contexts)))
         (use-ctx (= (length (--filter
                              (string= (plist-get it ':type)
                                       (and usable-ctxs (plist-get (car usable-ctxs) :type)))
                              usable-ctxs))
                     (length usable-ctxs))))

    (when (and usable-ctxs use-ctx)
      (plist-get (car usable-ctxs) :type))))

(defun dumber-jump-arg-joiner (prefix values)
  "Helper to generate command arg with its PREFIX for each value in VALUES."
  (let ((args (s-join (format " %s " prefix) values)))
    (if (and args values)
        (format " %s %s " prefix args)
      "")))

(defun dumber-jump-get-contextual-regexes (lang ctx-type searcher)
  "Get list of search regular expressions by LANG and CTX-TYPE (variable, function, etc)."
  (let* ((raw-rules (dumber-jump-get-rules-by-language lang searcher))
         (ctx-type (unless dumber-jump-ignore-context ctx-type))
         (ctx-rules
          (if ctx-type
              (--filter (string= (plist-get it :type) ctx-type) raw-rules)
            raw-rules))
         (rules (or ctx-rules raw-rules))
         (regexes (--map (plist-get it :regex) rules)))
    regexes))

(defun dumber-jump-populate-regex (it look-for variant)
  "Populate IT regex template with LOOK-FOR."
  (when (not (eq variant 'rg))
    (error "NO more support for %s" variant))
  (let ((boundary dumber-jump-rg-word-boundary))
    (let ((text it))
      (setq text (s-replace "\\j" boundary text))
      (setq text (s-replace "JJJ" (regexp-quote look-for) text))
      (when (and (eq variant 'rg) (string-prefix-p "-" text))
        (setq text (concat "[-]" (substring text 1))))
      text)))

(defun dumber-jump-populate-regexes (look-for regexes variant)
  "Take list of REGEXES and populate the LOOK-FOR target and return that list."
  (--map (dumber-jump-populate-regex it look-for variant) regexes))

(defun dumber-jump-generate-rg-command (look-for _cur-file proj regexes lang exclude-paths)
  "Generate the rg response based on the needle LOOK-FOR in the directory PROJ."
  (let* ((filled-regexes (dumber-jump-populate-regexes look-for regexes 'rg))
         (rgtypes (dumber-jump-get-rg-type-by-language lang))
         (proj-dir (file-name-as-directory proj))
         (cmd (concat dumber-jump-rg-cmd
                      " --color never --no-heading --line-number -U"
                      (when (not (s-blank? dumber-jump-rg-search-args))
                        (concat " " dumber-jump-rg-search-args))
                      (s-join "" (--map (format " --type %s" it) rgtypes))))
         (exclude-args (dumber-jump-arg-joiner
                        "-g" (--map (shell-quote-argument (concat "!" (s-replace proj-dir "" it))) exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (= (length regexes) 0)
        ""
      (dumber-jump-concat-command cmd exclude-args regex-args proj))))

(defun dumber-jump-concat-command (&rest parts)
  "Concat the PARTS of a command if each part has a length."
  (s-join " " (-map #'s-trim (--filter (> (length it) 0) parts))))

(defun dumber-jump-get-rg-type-by-language (language)
  "Return list of rg type argument for a LANGUAGE."
  (-distinct (--map (plist-get it :rgtype)
                    (--filter (and
                               (plist-get it :rgtype)
                               (string= (plist-get it :language) language))
                              dumber-jump-language-file-exts))))

(defun dumber-jump-get-rules-by-language (language _searcher)
  "Return a list of rules for the LANGUAGE by SEARCHER."
  (let* ((searcher-str "rg")
         (results (--filter (and
                             (string= (plist-get it ':language) language)
                             (member searcher-str (plist-get it ':supports)))
                            dumber-jump-find-rules)))
    (if dumber-jump-functions-only
        (--filter (string= (plist-get it ':type) "function") results)
      results)))

(when (featurep 'xref)
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql dumber-jump)))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (and bounds (let* ((ident (dumber-jump-get-point-symbol))
                         (start (car bounds))
                         (col (- start (line-beginning-position)))
                         (line (dumber-jump-get-point-line))
                         (ctx (dumber-jump-get-point-context line ident col)))
                    (propertize ident :dumber-jump-ctx ctx)))))

  (cl-defmethod xref-backend-definitions ((_backend (eql dumber-jump)) prompt)
    (let* ((info (dumber-jump-get-results prompt))
           (results (plist-get info :results))
           (look-for (or prompt (plist-get info :symbol)))
           (proj-root (plist-get info :root))
           (issue (plist-get info :issue))
           (lang (plist-get info :lang))
           (processed (dumber-jump-process-results
                       results
                       (plist-get info :file)
                       proj-root
                       (plist-get info :ctx-type)
                       look-for
                       nil
                       nil))
           (results (plist-get processed :results))
           (do-var-jump (plist-get processed :do-var-jump))
           (var-to-jump (plist-get processed :var-to-jump))
           (match-cur-file-front (plist-get processed :match-cur-file-front)))

      (dumber-jump-debug-message
        look-for
        (plist-get info :ctx-type)
        var-to-jump
        (pp-to-string match-cur-file-front)
        (pp-to-string results)
        match-cur-file-front
        proj-root
        (plist-get info :file))
      (cond ((eq issue 'nogrep)
             (dumber-jump-message "Please install rg!"))
            ((eq issue 'nosymbol)
             (dumber-jump-message "No symbol under point."))
            ((s-ends-with? " file" lang)
             (dumber-jump-message "Could not find rules for '%s'." lang))
            ((= (length results) 0)
             (dumber-jump-message "'%s' %s %s declaration not found." look-for (if (s-blank? lang) "with unknown language so" lang) (plist-get info :ctx-type)))
            (t (mapcar (lambda (res)
                         (xref-make
                          (plist-get res :context)
                          (xref-make-file-location
                           (expand-file-name (plist-get res :path))
                           (plist-get res :line)
                           0)))
                       (if do-var-jump
                           (list var-to-jump)
                         match-cur-file-front))))))

  (cl-defmethod xref-backend-apropos ((_backend (eql dumber-jump)) pattern)
    (xref-backend-definitions 'dumber-jump pattern))

  (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql dumber-jump)))
    nil))

;;;###autoload
(defun dumber-jump-xref-activate ()
  "Function to activate xref backend.
Add this function to `xref-backend-functions' to dumber jump to be
activiated, whenever it finds a project. It is recommended to add
it to the end, so that it only gets activated when no better
option is found."
  (and (dumber-jump-get-project-root default-directory)
       'dumber-jump))

(provide 'dumber-jump)
;;; dumber-jump.el ends here
