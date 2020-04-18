;;; dumb-jump.el --- Jump to definition for 40+ languages without configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019 jack angers
;; Author: jack angers and contributors
;; Url: https://github.com/jacktasia/dumb-jump
;; Version: 0.5.3
;; Package-Requires: ((emacs "24.4"))
;; Keywords: programming

;; Dumb Jump is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Dumb Jump is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Dumb Jump.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Dumb Jump is an Emacs "jump to definition" package with support for 40+
;; programming languages that favors "just working" over speed or accuracy. This
;; means minimal -- and ideally zero -- configuration with absolutely no stored
;; indexes (TAGS) or persistent background processes. Dumb Jump performs best
;; with The Silver Searcher (ag) or ripgrep (rg) installed.

;;; Code:

(require 'ring)
(require 'etags)
(require 'cl-lib)
(require 'eieio)
(require 'tramp)

(defgroup dumb-jump nil
  "Easily jump to project function and variable definitions"
  :group 'tools
  :group 'convenience)


;;; User Options

(defcustom dumb-jump-window 'current
  "Which window to use when jumping.  Valid options are 'current (default) or 'other."
  :type '(choice (const :tag "Current window" current)
          (const :tag "Other window" other)))

(defcustom dumb-jump-use-visible-window t
  "When true will jump in a visible window if that window already has the file open."
  :type 'boolean)

(defcustom dumb-jump-prefer-searcher nil
  "The preferred searcher to use 'ag, 'rg, 'git-grep, 'gnu-grep,or 'grep.
If nil then the most optimal searcher will be chosen at runtime."
  :type '(choice (const :tag "Best Available" nil)
          (const :tag "ag" dumb-jump-ag)
          (const :tag "rg" dumb-jump-rg)
          (const :tag "grep" dumb-jump-gnu-grep)
          (const :tag "git grep" dumb-jump-git-grep)
          (const :tag "git grep + ag" dumb-jump-git-grep+ag)))

(defcustom dumb-jump-force-searcher nil
  "Forcibly use searcher: 'ag, 'rg, 'git-grep, 'gnu-grep, or 'grep.
Set to nil to not force anything and use `dumb-jump-prefer-searcher'
or most optimal searcher."
  :type '(choice (const :tag "Best Available" nil)
          (const :tag "ag" dumb-jump-ag)
          (const :tag "rg" dumb-jump-rg)
          (const :tag "grep" dumb-jump-gnu-grep)
          (const :tag "git grep" dumb-jump-git-grep)
          (const :tag "git grep + ag" dumb-jump-git-grep+ag)))

(defcustom dumb-jump-grep-prefix "LANG=C"
  "Prefix to grep command.  Seemingly makes it faster for pure text."
  :type 'string)

(defcustom dumb-jump-grep-cmd grep-program
  "The path to grep."
  :type 'string)

(defcustom dumb-jump-ag-cmd "ag"
  "The the path to the silver searcher.  By default assumes it is in path.  If not found fallbacks to grep."
  :type 'string)

(defcustom dumb-jump-rg-cmd "rg"
  "The the path to ripgrep.  By default assumes it is in path.  If not found fallbacks to grep."
  :type 'string)

(defcustom dumb-jump-git-cmd "git"
  "The the path to git grep.  By default assumes it is in path.  If not found fallbacks to grep."
  :type 'string)

(defcustom dumb-jump-zgrep-cmd "zgrep"
  "The path to grep to use for gzipped files.  By default assumes it is in path."
  :type 'string)

(defcustom dumb-jump-fallback-regex '("\\b" term word-boundary)
  "Regular expression to use when more complex ones fail."
  :type 'string)

(defcustom dumb-jump-fallback-search t
  "If nothing is found with normal search fallback to searching the fallback regex."
  :type 'boolean)

(defcustom dumb-jump-grep-args '("-REn")
  "Grep command args [R]ecursive, [E]xtended regexes, and show line [n]umbers."
  :type '(repeat string))

(defcustom dumb-jump-gnu-grep-args '("-rEn")
  "Grep command args [r]ecursive and [E]xtended regexes, and show line [n]umbers."
  :type '(repeat string))

(defcustom dumb-jump-max-find-time 2
  "Number of seconds a grep/find command can take before being warned to use ag and config."
  :type 'integer)

(defcustom dumb-jump-functions-only nil
  "Should we only jump to functions?"
  :type 'boolean)

(defcustom dumb-jump-quiet t
  "If non-nil, prevent messages from being printed."
  :type 'boolean)

(defcustom dumb-jump-ignore-context nil
  "If non-nil Dumb Jump will ignore the context of point when jumping."
  :type 'boolean)

(defcustom dumb-jump-git-grep-search-untracked t
  "If non-nil Dumb Jump will also search untracked files when using searcher git-grep."
  :type 'boolean)

(defcustom dumb-jump-git-grep-search-args nil
  "Appends the passed arguments to the git-grep search function."
  :type '(repeat string))

(defcustom dumb-jump-ag-search-args nil
  "Appends the passed arguments to the ag search function."
  :type '(repeat string))

(defcustom dumb-jump-rg-search-args nil
  "Appends the passed arguments to the rg search function."
  :type '(repeat string))

(defcustom dumb-jump-find-rules
  '((:type function
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\((defun|cl-defun)\\s+" term word-boundary)
     :tests ("(defun test (blah)" "(defun test\n" "(cl-defun test (blah)" "(cl-defun test\n")
     :not ("(defun test-asdf (blah)" "(defun test-blah\n" "(cl-defun test-asdf (blah)"
           "(cl-defun test-blah\n"  "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(defvar\\b\\s*" term word-boundary)
     :tests ("(defvar test " "(defvar test\n")
     :not ("(defvar tester" "(defvar test?" "(defvar test-"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(defcustom\\b\\s*" term word-boundary)
     :tests ("(defcustom test " "(defcustom test\n")
     :not ("(defcustom tester" "(defcustom test?" "(defcustom test-"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(setq\\b\\s*" term word-boundary)
     :tests ("(setq test 123)")
     :not ("setq test-blah 123)" "(setq tester" "(setq test?" "(setq test-"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(" term "\\s+")
     :tests ("(let ((test 123)))")
     :not ("(let ((test-2 123)))"))

    ;; variable in method signature
    (:type variable
     :supports (ag rg git-grep)
     :language elisp
     :regex ("\\((defun|cl-defun)\\s*.+\\\(?\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)")
     :not ("(defun blah (test-1)" "(defun blah (test-2 blah)" "(defun (blah test-3)"))

    ;; common lisp
    (:type function
     :supports (ag grep rg git-grep)
     :language commonlisp
     :regex ("\\\(defun\\s+" term word-boundary)
     :tests ("(defun test (blah)" "(defun test\n")
     :not ("(defun test-asdf (blah)" "(defun test-blah\n"
           "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language commonlisp
     :regex ("\\\(defparameter\\b\\s*" term word-boundary)
     :tests ("(defparameter test " "(defparameter test\n")
     :not ("(defparameter tester" "(defparameter test?" "(defparameter test-"))

    ;; racket
    (:type function
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+\\(\\s*" term word-boundary)
     :tests ("(define (test blah)" "(define (test\n")
     :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))
    (:type function
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+" term "\\s*\\\(\\s*lambda")
     :tests ("(define test (lambda (blah" "(define test (lambda\n")
     :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))
    (:type function
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(let\\s+" term "\\s*(\\\(|\\\[)*")
     :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
     :not ("(let ((test blah"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+" term word-boundary)
     :tests ("(define test " "(define test\n")
     :not ("(define (test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("(\\\(|\\\[)\\s*" term "\\s+")
     :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
     :not ("{test foo"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(lambda\\s+\\\(?[^\(\)]*\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
     :not ("(lambda () test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+\\\([^\(\)]+\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(define (foo test)" "(define (foo test bar)")
     :not ("(define foo test" "(define (test foo" "(define (test)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\(struct\\s+" term word-boundary)
     :tests ("(struct test (a b)"))

    ;; scheme
    (:type function
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+\\(\\s*" term word-boundary)
     :tests ("(define (test blah)" "(define (test\n")
     :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+" term "\\s*\\\(\\s*lambda")
     :tests ("(define test (lambda (blah" "(define test (lambda\n")
     :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(let\\s+" term "\\s*(\\\(|\\\[)*")
     :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
     :not ("(let ((test blah"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+" term word-boundary)
     :tests ("(define test " "(define test\n")
     :not ("(define (test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("(\\\(|\\\[)\\s*" term "\\s+")
     :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
     :not ("{test foo"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(lambda\\s+\\\(?[^\(\)]*\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
     :not ("(lambda () test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+\\\([^\(\)]+\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(define (foo test)" "(define (foo test bar)")
     :not ("(define foo test" "(define (test foo" "(define (test)"))

    ;; c++
    (:type function
     :supports (ag rg git-grep)
     :language c++
     :regex ("\\b" term "(\\s|\\))*\\((\\w|[,&*.<>]|\\s)*(\\))\\s*(const|->|\\{|$)|typedef\\s+(\\w|[(*]|\\s)+" term "(\\)|\\s)*\\(")
     :tests ("int test(){" "my_struct (*test)(int a, int b){" "auto MyClass::test ( Builder& reference, ) -> decltype( builder.func() ) {" "int test( int *random_argument) const {" "test::test() {" "typedef int (*test)(int);")
     :not ("return test();)" "int test(a, b);" "if( test() ) {" "else test();"))
    ;; (:type variable :supports (grep) :language c++
    ;;        :regex "(\\b\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[([0-9]|\\s)*\\])*\\s*([=,){;]|:\\s*[0-9])|#define\\s+JJJ\\b"
    ;;        :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "unsigned int test:2;"))
    (:type variable
     :supports (ag rg)
     :language c++
     :regex ("\\b(?!(class\\b|struct\\b|return\\b|else\\b|delete\\b))(\\w+|[,>])([*&]|\\s)+" term "\\s*(\\[(\\d|\\s)*\\])*\\s*([=,(){;]|:\\s*\\d)|#define\\s+" term "\\b")
     :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "typedef int test;" "unsigned int test:2")
     :not ("return test;" "#define NOT test" "else test=2;"))
    (:type type
     :supports (ag rg git-grep)
     :language c++
     :regex ("\\b(class|struct|enum|union)\\b\\s*" term "\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*" term "\\b\\s*;")
     :tests ("typedef struct test {" "enum test {" "} test;" "union test {" "class test final: public Parent1, private Parent2{" "class test : public std::vector<int> {")
     :not("union test var;" "struct test function() {"))

    ;; clojure
    (:type variable
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(def\\s+" term word-boundary)
     :tests ("(def test (foo)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defn-?\\s+" term word-boundary)
     :tests ("(defn test [foo]" "(defn- test [foo]")
     :not ("(defn test? [foo]" "(defn- test? [foo]"))
    (:type function
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defmacro\\s+" term word-boundary)
     :tests ("(defmacro test [foo]"))
    (:type function
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(deftask\\s+" term word-boundary)
     :tests ("(deftask test [foo]"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(deftype\\s+" term word-boundary)
     :tests ("(deftype test [foo]"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defmulti\\s+" term word-boundary)
     :tests ("(defmulti test fn"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defmethod\\s+" term word-boundary)
     :tests ("(defmethod test type"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(definterface\\s+" term word-boundary)
     :tests ("(definterface test (foo)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defprotocol\\s+" term word-boundary)
     :tests ("(defprotocol test (foo)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defrecord\\s+" term word-boundary)
     :tests ("(defrecord test [foo]"))

    ;; coffeescript
    (:type function
     :supports (ag grep rg git-grep)
     :language coffeescript
     :regex ("^\\s*" term "\\s*[=:].*[-=]>")
     :tests ("test = ()  =>" "test= =>" "test = ->" "test=()->"
             "test : ()  =>" "test: =>" "test : ->" "test:()->")
     :not ("# test = =>" "test = 1"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language coffeescript
     :regex ("^\\s*" term "\\s*[:=][^:=-][^>]+$")
     :tests ("test = $" "test : [" "test = {" "test = a")
     :not ("test::a" "test: =>" "test == 1" "# test = 1"))
    (:type class
     :supports (ag grep rg git-grep)
     :language coffeescript
     :regex ("^\\s*\\bclass\\s+" term "")
     :tests ("class test" "class test extends")
     :not ("# class"))

    ;; obj-c
    (:type function
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("\\\)\\s*" term "(:|\\b|\\s)")
     :tests ("- (void)test" "- (void)test:(UIAlertView *)alertView")
     :not ("- (void)testnot" "- (void)testnot:(UIAlertView *)alertView"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("\\b\\*?" term "\\s*=[^=\\n]+")
     :tests ("NSString *test = @\"asdf\"")
     :not ("NSString *testnot = @\"asdf\"" "NSString *nottest = @\"asdf\""))
    (:type type
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("(@interface|@protocol|@implementation)\\b\\s*" term "\\b\\s*")
     :tests ("@interface test: UIWindow")
     :not ("@interface testnon: UIWindow"))
    (:type type
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("typedef\\b\\s+(NS_OPTIONS|NS_ENUM)\\b\\([^,]+?,\\s*" term "\\b\\s*")
     :tests ("typedef NS_ENUM(NSUInteger, test)")
     :not ("typedef NS_ENUMD(NSUInteger, test)"))

    ;; swift
    (:type variable
     :supports (ag grep rg git-grep)
     :language swift
     :regex ("(let|var)\\s*" term "\\s*(=|:)[^=:\\n]+")
     :tests ("let test = 1234" "var test = 1234" "private lazy var test: UITapGestureRecognizer") :not ("if test == 1234:"))
    (:type function
     :supports (ag grep rg git-grep)
     :language swift
     :regex ("func\\s*" term "\\b\\s*\\\(")
     :tests ("func test(asdf)" "func test()")
     :not ("func testnot(asdf)" "func testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language swift
     :regex ("(class|struct)\\s*" term "\\b\\s*?")
     :tests ("class test:" "class test: UIWindow")
     :not ("class testnot:" "class testnot(object):"))

    ;; c#
    (:type function
     :supports (ag rg)
     :language csharp
     :regex ("^\\s*(?:[\\w\\[\\]]+\\s+){1,3}" term "\\s*\\\(")
     :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
             "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
     :not ("test()" "testnot()" "blah = new test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language csharp
     :regex ("\\s*\\b" term "\\s*=[^=\\n)]+")
     :tests ("int test = 1234")
     :not ("if test == 1234:" "int nottest = 44"))
    (:type type
     :supports (ag grep rg git-grep)
     :language csharp
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test:" "public class test : IReadableChannel, I")
     :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; java (literally the same regexes as c#, but different tests)
    (:type function
     :supports (ag rg)
     :language java
     :regex ("^\\s*(?:[\\w\\[\\]]+\\s+){1,3}" term "\\s*\\\(")
     :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
             "public static MyType test()" "private virtual SomeType test(param)" "static int test()"
             "private foo[] test()")
     :not ("test()" "testnot()" "blah = new test()" "foo bar = test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language java
     :regex ("\\s*\\b" term "\\s*=[^=\\n)]+")
     :tests ("int test = 1234")
     :not ("if test == 1234:" "int nottest = 44"))
    (:type type
     :supports (ag grep rg git-grep)
     :language java
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test:" "public class test implements Something")
     :not ("class testnot:" "public class testnot implements Something"))

    ;; vala (again just like c#, exactly the same..)
    (:type function
     :supports (ag rg)
     :language vala
     :regex ("^\\s*(?:[\\w\\[\\]]+\\s+){1,3}" term "\\s*\\\(")
     :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
             "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
     :not ("test()" "testnot()" "blah = new test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language vala
     :regex ("\\s*\\b" term "\\s*=[^=\\n)]+")
     :tests ("int test = 1234")
     :not ("if test == 1234:" "int nottest = 44"))
    (:type type
     :supports (ag grep rg git-grep)
     :language vala
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test:" "public class test : IReadableChannel, I")
     :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; coq
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Variable\\s+" term "\\b")
     :tests ("Variable test")
     :not ("Variable testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Inductive\\s+" term "\\b")
     :tests ("Inductive test")
     :not ("Inductive testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Lemma\\s+" term "\\b")
     :tests ("Lemma test")
     :not ("Lemma testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Definition\\s+" term "\\b")
     :tests ("Definition test")
     :not ("Definition testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Hypothesis\\s+" term "\\b")
     :tests ("Hypothesis test")
     :not ("Hypothesis testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Theorm\\s+" term "\\b")
     :tests ("Theorm test")
     :not ("Theorm testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Fixpoint\\s+" term "\\b")
     :tests ("Fixpoint test")
     :not ("Fixpoint testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Module\\s+" term "\\b")
     :tests ("Module test")
     :not ("Module testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*CoInductive\\s+" term "\\b")
     :tests ("CoInductive test")
     :not ("CoInductive testx"))

    ;; python
    (:type variable
     :supports (ag grep rg git-grep)
     :language python
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234:" "_test = 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language python
     :regex ("def\\s*" term "\\b\\s*\\\(")
     :tests ("\tdef test(asdf)" "def test()")
     :not ("\tdef testnot(asdf)" "def testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language python
     :regex ("class\\s*" term "\\b\\s*\\\(?")
     :tests ("class test(object):" "class test:")
     :not ("class testnot:" "class testnot(object):"))

    ;; matlab
    (:type variable
     :supports (ag grep rg git-grep)
     :language matlab
     :regex ("^\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("for test = 1:2:" "_test = 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language matlab
     :regex ("^\\s*function\\s*[^=]+\\s*=\\s*" term "\\b")
     :tests ("\tfunction y = test(asdf)" "function x = test()" "function [x, losses] = test(A, y, lambda, method, qtile)")
     :not ("\tfunction testnot(asdf)" "function testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language matlab
     :regex ("^\\s*classdef\\s*" term "\\b\\s*")
     :tests ("classdef test")
     :not ("classdef testnot"))

    ;; nim
    (:type variable
     :supports (ag grep rg git-grep)
     :language nim
     :regex ("(const|let|var)\\s*" term "\\s*(=|:)[^=:\\n]+")
     :tests ("let test = 1234" "var test = 1234" "var test: Stat" "const test = 1234")
     :not ("if test == 1234:"))
    (:type function
     :supports (ag grep rg git-grep)
     :language nim
     :regex ("(proc|func|macro|template)\\s*`?" term "`?\\b\\s*\\\(")
     :tests ("\tproc test(asdf)" "proc test()" "func test()" "macro test()" "template test()")
     :not ("\tproc testnot(asdf)" "proc testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language nim
     :regex ("type\\s*" term "\\b\\s*(\\{[^}]+\\})?\\s*=\\s*\\w+")
     :tests ("type test = object" "type test {.pure.} = enum")
     :not ("type testnot = object"))

    ;; nix
    (:type variable
     :supports (ag grep rg git-grep)
     :language nix
     :regex ("\\b\\s*" term "\\s*=[^=;]+")
     :tests ("test = 1234;" "test = 123;" "test=123")
     :not ("testNot = 1234;" "Nottest = 1234;" "AtestNot = 1234;"))

    ;; ruby
    (:type variable
     :supports (ag rg git-grep)
     :language ruby
     :regex ("^\\s*((\\w+[.])*\\w+,\\s*)*" term "(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)")
     :tests ("test = 1234" "self.foo, test, bar = args")
     :not ("if test == 1234" "foo_test = 1234"))
    (:type function
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*" term "($|[^\\w|:])")
     :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
             "def self.test()" "def MODULE::test()" "private def test")
     :not ("def test_foo"))
    (:type function
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|\\W)define(_singleton|_instance)?_method(\\s|[(])\\s*:" term "($|[^\\w|:])")
     :tests ("define_method(:test, &body)"
             "mod.define_instance_method(:test) { body }"))
    (:type type
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|[^\\w.])class\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("class test" "class Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|[^\\w.])module\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("module test" "module Foo::test"))
    (:type function
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|\\W)alias(_method)?\\W+" term "(\\W|$)")
     :tests ("alias test some_method"
             "alias_method :test, :some_method"
             "alias_method 'test' 'some_method'"
             "some_class.send(:alias_method, :test, :some_method)")
     :not ("alias some_method test"
           "alias_method :some_method, :test"
           "alias test_foo test"))

    ;; Groovy
    (:type variable
     :supports (ag rg git-grep)
     :language groovy
     :regex ("^\\s*((\\w+[.])*\\w+,\\s*)*" term "(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)")
     :tests ("test = 1234" "self.foo, test, bar = args")
     :not ("if test == 1234" "foo_test = 1234"))
    (:type function
     :supports (ag rg git-grep)
     :language groovy
     :regex ("(^|[^\\w.])((private|public)\\s+)?def\\s+(\\w+(::|[.]))*" term "($|[^\\w|:])")
     :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
             "def self.test()" "def MODULE::test()" "private def test")
     :not ("def test_foo"))
    (:type type :supports (ag rg git-grep) :language groovy
     :regex ("(^|[^\\w.])class\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("class test" "class Foo::test"))

    ;; crystal
    (:type variable
     :supports (ag rg git-grep)
     :language crystal
     :regex ("^\\s*((\\w+[.])*\\w+,\\s*)*" term "(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)")
     :tests ("test = 1234" "self.foo, test, bar = args")
     :not ("if test == 1234" "foo_test = 1234"))
    (:type function
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*" term "($|[^\\w|:])")
     :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
             "def self.test()" "def MODULE::test()" "private def test")
     :not ("def test_foo"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])class\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("class test" "class Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])module\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("module test" "module Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])struct\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("struct test" "struct Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])alias\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("alias test" "alias Foo::test"))

    ;; scad
    (:type variable
     :supports (ag grep rg git-grep)
     :language scad
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234 {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scad
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type module
     :supports (ag grep rg git-grep)
     :language scad
     :regex ("module\\s*" term "\\s*\\\(")
     :tests ("module test()" "module test ()"))

    ;; scala
    (:type variable
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\bval\\s*" term "\\s*=[^=\\n]+")
     :tests ("val test = 1234")
     :not ("case test => 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\bvar\\s*" term "\\s*=[^=\\n]+")
     :tests ("var test = 1234")
     :not ("case test => 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\btype\\s*" term "\\s*=[^=\\n]+")
     :tests ("type test = 1234")
     :not ("case test => 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\bdef\\s*" term "\\s*\\\(")
     :tests ("def test(asdf)" "def test()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("class\\s*" term "\\s*\\\(?")
     :tests ("class test(object)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("trait\\s*" term "\\s*\\\(?")
     :tests ("trait test(object)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("object\\s*" term "\\s*\\\(?")
     :tests ("object test(object)"))

    ;; R
    (:type variable
     :supports (ag grep rg git-grep)
     :language r
     :regex ("\\b" term "\\s*=[^=><]")
     :tests ("test = 1234")
     :not ("if (test == 1234)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language r
     :regex ("\\b" term "\\s*<-\\s*function\\b")
     :tests ("test <- function" "test <- function(")
     :not   ("test <- functionX"))

    ;; perl
    (:type function
     :supports (ag grep rg git-grep)
     :language perl
     :regex ("sub\\s*" term "\\s*(\\{|\\()")
     :tests ("sub test{" "sub test {" "sub test(" "sub test ("))
    (:type variable
     :supports (ag grep rg git-grep)
     :language perl
     :regex ("" term "\\s*=\\s*")
     :tests ("$test = 1234"))

    ;; shell
    :type function
    :supports (ag grep rg git-grep)
    :language shell
    :regex ("function\\s*" term "\\s*")
    :tests ("function test{" "function test {" "function test () {")
    :not   ("function nottest {")
    (:type function
     :supports (ag grep rg git-grep)
     :language shell
     :regex ("" term "\\\(\\\)\\s*\\{")
     :tests ("test() {")
     :not ("testx() {"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language shell
     :regex ("\\b" term "\\s*=\\s*")
     :tests ("test = 1234") :not ("blahtest = 1234"))

    ;; php
    (:type function
     :supports (ag grep rg git-grep)
     :language php
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language php
     :regex ("\\*\\s@method\\s+[^   ]+\\s+" term "\\(")
     :tests ("/** @method string|false test($a)" " * @method bool test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language php
     :regex ("(\\s|->|\\$|::)" term "\\s*=\\s*")
     :tests ("$test = 1234" "$foo->test = 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language php
     :regex ("\\*\\s@property(-read|-write)?\\s+([^     ]+\\s+)&?\\$" term "(\\s+|$)")
     :tests ("/** @property string $test" "/** @property string $test description for $test property"  " * @property-read bool|bool $test" " * @property-write \\ArrayObject<string,resource[]> $test"))
    (:type trait
     :supports (ag grep rg git-grep)
     :language php
     :regex ("trait\\s*" term "\\s*\\\{")
     :tests ("trait test{" "trait test {"))
    (:type interface
     :supports (ag grep rg git-grep)
     :language php
     :regex ("interface\\s*" term "\\s*\\\{")
     :tests ("interface test{" "interface test {"))
    (:type class
     :supports (ag grep rg git-grep)
     :language php
     :regex ("class\\s*" term "\\s*(extends|implements|\\\{)")
     :tests ("class test{" "class test {" "class test extends foo" "class test implements foo"))

    ;; dart
    (:type function
     :supports (ag grep rg git-grep)
     :language dart
     :regex ("\\b" term "\\s*\\([^()]*\\)\\s*[{]")
     :tests ("test(foo) {" "test (foo){" "test(foo){"))
    (:type function
     :supports (ag grep rg git-grep)
     :language dart
     :regex ("class\\s*" term "\\s*[\\\(\\\{]")
     :tests ("class test(object) {" "class test{"))

    ;; faust
    (:type function
     :supports (ag grep rg git-grep)
     :language faust
     :regex ("\\b" term "\(\\\(.+\\\)\)*\\s*=")
     :tests ("test = osc + 0.5;" "test(freq) = osc(freq) + 0.5;"))

    ;; fortran
    (:type variable
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if (test == 1234)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("\\b(function|subroutine|FUNCTION|SUBROUTINE)\\s+" term "\\b\\s*\\\(")
     :tests ("function test (foo)" "integer function test(foo)"
             "subroutine test (foo, bar)" "FUNCTION test (foo)"
             "INTEGER FUNCTION test(foo)" "SUBROUTINE test (foo, bar)")
     :not ("end function test" "end subroutine test" "END FUNCTION test"
           "END SUBROUTINE test"))
    (:type function
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("^\\s*(interface|INTERFACE)\\s+" term "\\b")
     :tests ("interface test" "INTERFACE test")
     :not ("interface test2" "end interface test" "INTERFACE test2"
           "END INTERFACE test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("^\\s*(module|MODULE)\\s+" term "\\s*")
     :tests ("module test" "MODULE test")
     :not ("end module test" "END MODULE test"))

    ;; go
    (:type variable
     :supports (ag grep rg git-grep)
     :language go
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234 {"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language go
     :regex ("\\s*\\b" term "\\s*:=\\s*")
     :tests ("test := 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language go
     :regex ("func\\s+\\\([^\\\)]*\\\)\\s+" term "\\s*\\\(")
     :tests ("func (s *blah) test(filename string) string {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language go
     :regex ("func\\s+" term "\\s*\\\(")
     :tests ("func test(url string) (string, error)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language go
     :regex ("type\\s+" term "\\s+struct\\s+\\\{")
     :tests ("type test struct {"))

    ;; javascript extended
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("(service|factory)\\\(['\"]" term "['\"]")
     :tags ("angular")
     :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>")
     :tags ("es6")
     :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*\\([^()]*\\)\\s*[{]")
     :tags ("es6")
     :tests ("test(foo) {" "test (foo){" "test(foo){")
     :not ("test = blah.then(function(){"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :tags ("es6")
     :regex ("class\\s*" term "\\s*[\\\(\\\{]")
     :tests ("class test(object) {" "class test{"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :tags ("es6")
     :regex ("class\\s*" term "\\s+extends")
     :tests ("class test extends Component{"))

    ;; hcl terraform
    (:type block
     :supports (ag grep rg git-grep)
     :language hcl
     :regex ("(variable|output|module)\\s*\"" term "\"\\s*\\\{")
     :tests ("variable \"test\" {"
             "output \"test\" {"
             "module \"test\" {"))
    (:type block
     :supports (ag grep rg git-grep)
     :language hcl
     :regex ("(data|resource)\\s*\"\\w+\"\\s*\"" term "\"\\s*\\\{")
     :tests ("data \"openstack_images_image_v2\" \"test\" {"
             "resource \"google_compute_instance\" \"test\" {"))

    ;; javascript
    (:type variable
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234" "const test = props =>")
     :not ("if (test === 1234)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\b" term "\\b\\s*,?\\s*\\\)?")
     :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
     :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
           "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*:\\s*function\\s*\\\(")
     :tests ("test: function()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*=\\s*function\\s*\\\(")
     :tests ("test = function()"))

    ;; typescript
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("(service|factory)\\\(['\"]" term "['\"]")
     :tags ("angular")
     :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>")
     :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*\\([^()]*\\)\\s*[{]")
     :tests ("test(foo) {" "test (foo){" "test(foo){")
     :not ("test = blah.then(function(){"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("class\\s*" term "\\s*[\\\(\\\{]")
     :tests ("class test{"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("class\\s*" term "\\s+extends")
     :tests ("class test extends Component{"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*:\\s*function\\s*\\\(")
     :tests ("test: function()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*=\\s*function\\s*\\\(")
     :tests ("test = function()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234" "const test = props =>")
     :not ("if (test === 1234)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\b" term "\\b\\s*,?\\s*\\\)?")
     :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
     :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
           "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

    ;; julia
    (:type function
     :supports (ag grep rg git-grep)
     :language julia
     :regex ("(@noinline|@inline)?\\s*function\\s*" term "(\\{[^\\}]*\\})?\\(")
     :tests ("function test()" "@inline function test()"
             "function test{T}(h)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language julia
     :regex ("(@noinline|@inline)?" term "(\\{[^\\}]*\\})?\\([^\\)]*\\)\s*=")
     :tests ("test(a)=1" "test(a,b)=1*8"
             "@noinline test()=1" "test{T}(x)=x"))
    (:type function
     :supports (ag grep rg git-grep)
     :language julia
     :regex ("macro\\s*" term "\\(")
     :tests ("macro test(a)=1" " macro test(a,b)=1*8"))
    (:type variable
     :supports (ag rg)
     :language julia
     :regex ("const\\s+" term "\\b")
     :tests ("const test = "))
    (:type type
     :supports (ag rg)
     :language julia
     :regex ("(mutable)?\\s*struct\\s*" term "")
     :tests ("struct test"))
    (:type type
     :supports (ag rg)
     :language julia
     :regex ("(type|immutable|abstract)\\s*" term "")
     :tests ("type test" "immutable test" "abstract test <:Testable" ))

    ;; haskell
    (:type module
     :supports (ag)
     :language haskell
     :regex ("^module\\s+" term "\\s+")
     :tests ("module Test (exportA, exportB) where"))
                                        ; TODO Doesn't support any '=' in arguments. E.g. 'foo A{a = b,..} = bar'.
    (:type top level function
     :supports (ag)
     :language haskell
     :regex ("^\\b" term "(?!(\\s+::))\\s+((.|\\s)*?)=\\s+")
     :tests ("test n = n * 2"
             "test X{..} (Y a b c) \n bcd \n =\n x * y"
             "test ab cd e@Datatype {..} (Another thing, inTheRow) = \n undefined"
             "test = runRealBasedMode @ext @ctx identity identity"
             "test unwrap wrap nr@Naoeu {..} (Action action, specSpecs) = \n    undefined")
     :not ("nottest n = n * 2"
           "let testnot x y = x * y" "test $ y z" "let test a o = mda"
           "test :: Sometype -> AnotherType aoeu kek = undefined"))
    (:type type-like
     :supports (ag)
     :language haskell
     :regex ("^\\s*((data(\\s+family)?)|(newtype)|(type(\\s+family)?))\\s+" term "\\s+")
     :tests ("newtype Test a = Something { b :: Kek }"
             "data Test a b = Somecase a | Othercase b"
             "type family Test (x :: *) (xs :: [*]) :: Nat where"
             "data family Test "
             "type Test = TestAlias")
     :not ("newtype NotTest a = NotTest (Not a)"
           "data TestNot b = Aoeu"))
                                        ; datatype contstuctor that doesn't match type definition.
    (:type (data)type constructor 1
     :supports (ag)
     :language haskell
     :regex ("(data|newtype)\\s{1,3}(?!" term "\\s+)([^=]{1,40})=((\\s{0,3}" term "\\s+)|([^=]{0,500}?((?<!(-- ))\\|\\s{0,3}" term "\\s+)))")
     :tests ("data Something a = Test { b :: Kek }"
             "data Mem a = TrueMem { b :: Kek } | Test (Mem Int) deriving Mda"
             "newtype SafeTest a = Test (Kek a) deriving (YonedaEmbedding)")
     :not ("data Test = Test { b :: Kek }"))
    (:type data/newtype record field
     :supports (ag)
     :language haskell
     :regex ("(data|newtype)([^=]*)=[^=]*?({([^=}]*?)(\\b" term ")\\s+::[^=}]+})")
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
    (:type typeclass
     :supports (ag)
     :language haskell
     :regex ("^class\\s+(.+=>\\s*)?" term "\\s+")
     :tests ("class (Constr1 m, Constr 2) => Test (Kek a) where"
             "class  Test  (Veryovka a)  where ")
     :not ("class Test2 (Kek a) where"
           "class MakeTest (AoeuTest x y z) where"))

    ;; ocaml
    (:type type
     :supports (ag rg)
     :language ocaml
     :regex ("^\\s*(and|type)\\s+.*\\b" term "\\b")
     :tests ("type test ="
             "and test ="
             "type 'a test ="
             "type ('a, _, 'c) test"))
    (:type variable
     :supports (ag rg)
     :language ocaml
     :regex ("let\\s+" term "\\b")
     :tests ("let test ="
             "let test x y ="))
    (:type variable
     :supports (ag rg)
     :language ocaml
     :regex ("let\\s+rec\\s+" term "\\b")
     :tests ("let rec test ="
             "let rec  test x y ="))
    (:type variable
     :supports (ag rg)
     :language ocaml
     :regex ("\\s*val\\s*\\b" term "\\b\\s*")
     :tests ("val test"))
    (:type module
     :supports (ag rg)
     :language ocaml
     :regex ("^\\s*module\\s*\\b" term "\\b")
     :tests ("module test ="))
    (:type module
     :supports (ag rg)
     :language ocaml
     :regex ("^\\s*module\\s*type\\s*\\b" term "\\b")
     :tests ("module type test ="))

    ;; lua
    (:type variable
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test === 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\b" term "\\b\\s*,?\\s*\\\)?")
     :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah)" "function(blah, test)")
     :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah)" "function(blah, testLast)"
           "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah)" "function(blah, Lasttest)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("function\\s*.+[.:]" term "\\s*\\\(")
     :tests ("function MyClass.test()" "function MyClass.test ()"
             "function MyClass:test()" "function MyClass:test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\b" term "\\s*=\\s*function\\s*\\\(")
     :tests ("test = function()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\b.+\\." term "\\s*=\\s*function\\s*\\\(")
     :tests ("MyClass.test = function()"))

    ;; rust
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\blet\\s+(\\\([^=\\n]*)?(mut\s+)?" term "([^=\\n]*\\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+")
     :tests ("let test = 1234;"
             "let test: u32 = 1234;"
             "let test: Vec<u32> = Vec::new();"
             "let mut test = 1234;"
             "let mut test: Vec<u32> = Vec::new();"
             "let (a, test, b) = (1, 2, 3);"
             "let (a, mut test, mut b) = (1, 2, 3);"
             "let (mut a, mut test): (u32, usize) = (1, 2);"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bconst\\s+" term ":\\s*[^=\\n]+\\s*=[^=\\n]+")
     :tests ("const test: u32 = 1234;"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bstatic\\s+(mut\\s+)?" term ":\\s*[^=\\n]+\\s*=[^=\\n]+")
     :tests ("static test: u32 = 1234;"
             "static mut test: u32 = 1234;"))
    ;; variable in method signature
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bfn\\s+.+\\s*\\\((.+,\\s+)?" term ":\\s*[^=\\n]+\\s*(,\\s*.+)*\\\)")
     :tests ("fn abc(test: u32) -> u32 {"
             "fn abc(x: u32, y: u32, test: Vec<u32>, z: Vec<Foo>)"
             "fn abc(x: u32, y: u32, test: &mut Vec<u32>, z: Vec<Foo>)"))
    ;; "if let" and "while let" desugaring
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("(if|while)\\s+let\\s+([^=\\n]+)?(mut\\s+)?" term "([^=\\n\\\(]+)?\\s*=\\s*[^=\\n]+")
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
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("struct\\s+[^\\n{]+[{][^}]*(\\s*" term "\\s*:\\s*[^\\n},]+)[^}]*}")
     :tesst ("struct Foo { abc: u32, test: Vec<String>, b: PathBuf }"
             "struct Foo<T>{test:Vec<T>}"
             "struct FooBar<'a> { test: Vec<String> }")
     :not ("struct Foo { abc: u32, b: Vec<String> }"
           "/// ... construct the equivalent ...\nfn abc() {\n"))
    ;; enum variants
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("enum\\s+[^\\n{]+\\s*[{][^}]*\\b" term "\\b[^}]*}")
     :tests ("enum Foo { VariantA, test, VariantB(u32) }"
             "enum Foo<T> { test(T) }"
             "enum BadStyle{test}"
             "enum Foo32 { Bar, testing, test(u8) }")
     :not ("enum Foo { testing }"))
    (:type function
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bfn\\s+" term "\\s*\\\(")
     :tests ("fn test(asdf: u32)" "fn test()" "pub fn test()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bmacro_rules!\\s+" term "")
     :tests ("macro_rules! test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("struct\\s+" term "\\s*[{\\\(]?")
     :tests ("struct test(u32, u32)"
             "struct test;"
             "struct test { abc: u32, def: Vec<String> }"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("trait\\s+" term "\\s*[{]?")
     :tests ("trait test;" "trait test { fn abc() -> u32; }"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\btype\\s+" term "([^=\\n]+)?\\s*=[^=\\n]+;")
     :tests ("type test<T> = Rc<RefCell<T>>;"
             "type test = Arc<RwLock<Vec<u32>>>;"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("impl\\s+((\\w+::)*\\w+\\s+for\\s+)?(\\w+::)*" term "\\s+[{]?")
     :tests ("impl test {"
             "impl abc::test {"
             "impl std::io::Read for test {"
             "impl std::io::Read for abc::test {"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("mod\\s+" term "\\s*[{]?")
     :tests ("mod test;" "pub mod test {"))

    ;; elixir
    (:type function
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("\\bdef(p)?\\s+" term "\\s*[ ,\\\(]")
     :tests ("def test do"
             "def test, do:"
             "def test() do"
             "def test(), do:"
             "def test(foo, bar) do"
             "def test(foo, bar), do:"
             "defp test do"
             "defp test(), do:"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("\\s*" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234"))
    (:type module
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("defmodule\\s+(\\w+\\.)*" term "\\s+")
     :tests ("defmodule test do"
             "defmodule Foo.Bar.test do"))
    (:type module
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("defprotocol\\s+(\\w+\\.)*" term "\\s+")
     :tests ("defprotocol test do"
             "defprotocol Foo.Bar.test do"))

    ;; erlang
    (:type function
     :supports (ag grep rg git-grep)
     :language erlang
     :regex ("^" term "\\b\\s*\\\(")
     :tests ("test() ->"
             "test()->"
             "test(Foo) ->"
             "test (Foo,Bar) ->"
             "test(Foo, Bar)->"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language erlang
     :regex ("\\s*" term "\\s*=[^:=\\n]+")
     :tests ("test = 1234")
     :not ("if test =:= 1234"
           "if test == 1234"))
    (:type module
     :supports (ag grep rg git-grep)
     :language erlang
     :regex ("^-module\\\(" term "\\\)")
     :tests ("-module(test)."))

    ;; scss
    (:type function
     :supports (ag grep rg git-grep)
     :language scss
     :regex ("@mixin\\s" term "\\b\\s*\\\(")
     :tests ("@mixin test()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scss
     :regex ("@function\\s" term "\\b\\s*\\\(")
     :tests ("@function test()"))
    (:type variable :supports (ag grep rg git-grep) :language scss
     :regex ("" term "\\s*:\\s*")
     :tests ("test  :"))

    ;; sml
    (:type type
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*(data)?type\\s+.*\\b" term "\\b")
     :tests ("datatype test ="
             "datatype test="
             "datatype 'a test ="
             "type test ="
             "type 'a test ="
             "type 'a test"
             "type test")
     :not ("datatypetest ="))
    (:type variable
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*val\\s+\\b" term "\\b")
     :tests ("val test ="
             "val test="
             "val test : bool"))
    (:type function
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*fun\\s+\\b" term "\\b.*\\s*=")
     :tests ("fun test list ="
             "fun test (STRING_NIL, a) ="
             "fun test ((s1,s2): 'a queue) : 'a * 'a queue ="
             "fun test (var : q) : int ="
             "fun test f e xs ="))
    (:type module
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*(structure|signature|functor)\\s+\\b" term "\\b")
     :tests ("structure test ="
             "structure test : MYTEST ="
             "signature test ="
             "functor test (T:TEST) ="
             "functor test(T:TEST) ="))

    ;; sql
    (:type function
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(FUNCTION|function|PROCEDURE|procedure)\\s+" term "\\s*\\\(")
     :tests ("CREATE FUNCTION test(i INT) RETURNS INT"
             "create or replace function test (int)"
             "CREATE PROCEDURE test (OUT p INT)"
             "create definer = 'test'@'localhost' procedure test()"))
    (:type table
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(TABLE|table)(\\s+(IF NOT EXISTS|if not exists))?\\s+" term "\\b")
     :tests ("CREATE TABLE test ("
             "create temporary table if not exists test"
             "CREATE TABLE IF NOT EXISTS test ("
             "create global temporary table test"))
    (:type view
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(VIEW|view)\\s+" term "\\b")
     :tests ("CREATE VIEW test ("
             "create sql security definer view test"
             "CREATE OR REPLACE VIEW test AS foo"))
    (:type type
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(TYPE|type)\\s+" term "\\b")
     :tests ("CREATE TYPE test"
             "CREATE OR REPLACE TYPE test AS foo ("
             "create type test as ("))

    ;; systemverilog
    (:type type
     :supports (ag grep rg git-grep)
     :language systemverilog
     :regex ("\\s*class\\s+\\b" term "\\b")
     :tests ("virtual class test;" "class test;" "class test extends some_class")
     :not ("virtual class testing;" "class test2;" "class some_test" "class some_class extends test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language systemverilog
     :regex ("\\s*task\\s+\\b" term "\\b")
     :tests ("task test (" "task test(")
     :not ("task testing (" "task test2("))
    (:type type
     :supports (ag grep rg git-grep)
     :language systemverilog
     :regex ("\\s*\\b" term "\\b\\s*=")
     :tests ("assign test ="
             "assign test="
             "int test ="
             "int test=")
     :not ("assign testing =" "assign test2="))
    (:type function
     :supports (ag rg git-grep)
     :language systemverilog
     :regex ("function\\s[^\\s]+\\s*\\b" term "\\b")
     :tests ("function Matrix test ;"
             "function Matrix test;")
     :not ("function test blah"))
    ;; matches SV class handle declarations
    (:type function
     :supports (ag rg git-grep)
     :language systemverilog
     :regex ("^\\s*[^\\s]*\\s*[^\\s]+\\s+\\b" term "\\b")
     :tests ("some_class_name test"
             "  another_class_name  test ;"
             "some_class test[];"
             "some_class #(1) test")
     :not ("test some_class_name" "class some_class extends test"))

    ;; vhdl
    (:type type
     :supports (ag grep rg git-grep)
     :language vhdl
     :regex ("\\s*type\\s+\\b" term "\\b")
     :tests ("type test is" "type test  is")
     :not ("type testing is" "type test2  is"))
    (:type type
     :supports (ag grep rg git-grep)
     :language vhdl
     :regex ("\\s*constant\\s+\\b" term "\\b")
     :tests ("constant test :" "constant test:")
     :not ("constant testing " "constant test2:"))
    (:type function
     :supports (ag grep rg git-grep)
     :language vhdl
     :regex ("function\\s*\"?" term "\"?\\s*\\\(")
     :tests ("function test(signal)"
             "function test (signal)"
             "function \"test\" (signal)")
     :not ("function testing(signal"))

    ;; latex
    (:type command
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\.*newcommand\\\*?\\s*\\\{\\s*(\\\\)" term "\\s*}")
     :tests ("\\newcommand{\\test}"
             "\\renewcommand{\\test}"
             "\\renewcommand*{\\test}"
             "\\newcommand*{\\test}"
             "\\renewcommand{ \\test }")
     :not("\\test"  "test"))
    (:type command
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\.*newcommand\\\*?\\s*(\\\\)" term word-boundary)
     :tests ("\\newcommand\\test {}"
             "\\renewcommand\\test{}"
             "\\newcommand \\test")
     :not("\\test"  "test"))
    (:type length
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\(s)etlength\\s*\\\{\\s*(\\\\)" term "\\s*}")
     :tests ("\\setlength { \\test}"
             "\\setlength{\\test}"
             "\\setlength{\\test}{morecommands}" )
     :not("\\test"  "test"))
    (:type counter
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\newcounter\\\{\\s*" term "\\s*}")
     :tests ("\\newcounter{test}" )
     :not("\\test"  "test"))
    (:type environment
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\.*newenvironment\\s*\\\{\\s*" term "\\s*}")
     :tests ("\\newenvironment{test}"
             "\\newenvironment {test}{morecommands}"
             "\\lstnewenvironment{test}"
             "\\newenvironment {test}" )
     :not("\\test"  "test" ))

    ;; pascal (todo: var, type, const)
    (:type function
     :supports (ag grep rg git-grep)
     :language pascal
     :regex ("\\bfunction\\s+" term "\\b")
     :tests ("  function test : "))
    (:type function
     :supports (ag grep rg git-grep)
     :language pascal
     :regex ("\\bprocedure\\s+" term "\\b")
     :tests ("  procedure test ; "))

    ;; f#
    (:type variable
     :supports (ag grep git-grep)
     :language fsharp
     :regex ("let\\s+" term "\\b.*\\\=")
     :tests ("let test = 1234" "let test() = 1234" "let test abc def = 1234")
     :not ("let testnot = 1234"
           "let testnot() = 1234"
           "let testnot abc def = 1234"))
    (:type interface
     :supports (ag grep git-grep)
     :language fsharp
     :regex ("member(\\b.+\\.|\\s+)" term "\\b.*\\\=")
     :tests ("member test = 1234"
             "member this.test = 1234")
     :not ("member testnot = 1234"
           "member this.testnot = 1234"))
    (:type type
     :supports (ag grep git-grep)
     :language fsharp
     :regex ("type\\s+" term "\\b.*\\\=")
     :tests ("type test = 1234")
     :not ("type testnot = 1234"))

    ;; kotlin
    (:type function
     :supports (ag grep rg git-grep)
     :language kotlin
     :regex ("fun\\s*(<[^>]*>)?\\s*" term "\\s*\\(")
     :tests ("fun test()" "fun <T> test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language kotlin
     :regex ("(val|var)\\s*" term "\\b")
     :not ("val testval" "var testvar")
     :tests ("val test " "var test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language kotlin
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test" "class test : SomeInterface" "interface test"))

    ;; protobuf
    (:type message
     :supports (ag grep rg git-grep)
     :language protobuf
     :regex ("message\\s+" term "\\s*\\\{")
     :tests ("message test{" "message test {"))
    (:type enum
     :supports (ag grep rg git-grep)
     :language protobuf
     :regex ("enum\\s+" term "\\s*\\\{")
     :tests ("enum test{" "enum test {")))
  "List of regex patttern templates organized by language and type to use for generating the grep command."
  :type '(repeat (plist :options ((:type string)
                                  (:supports string)
                                  (:language string)
                                  (:regex string)
                                  (:tests (repeat string))
                                  (:not (repeat string))))))

(defcustom dumb-jump-language-file-exts
  ;; https://github.com/ggreer/the_silver_searcher/blob/master/tests/list_file_types.t
  ;; https://github.com/BurntSushi/ripgrep/blob/master/ignore/src/types.rs#L99
  '((:language elisp :ext "el" :agtype "elisp" :rgtype "elisp")
    (:language elisp :ext "el.gz" :agtype "elisp" :rgtype "elisp")
    (:language commonlisp :ext "lisp" :agtype "lisp" :rgtype "lisp")
    (:language commonlisp :ext "lsp" :agtype "lisp" :rgtype "lisp")
    (:language c++ :ext "c" :agtype "cc" :rgtype "c")
    (:language c++ :ext "h" :agtype "cc" :rgtype "c")
    (:language c++ :ext "C" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "H" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "tpp" :agtype "cpp")
    (:language c++ :ext "cpp" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "hpp" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "cxx" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "hxx" :agtype "cpp")
    (:language c++ :ext "cc" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "hh" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "c++")
    (:language c++ :ext "h++")
    (:language coq :ext "v")
    (:language ocaml :ext "ml" :agtype "ocaml" :rgtype "ocaml")
    (:language ocaml :ext "mli" :agtype "ocaml" :rgtype "ocaml")
    (:language ocaml :ext "mll" :agtype "ocaml" :rgtype "ocaml")
    (:language ocaml :ext "mly" :agtype "ocaml" :rgtype "ocaml")
    ;; groovy is nil type because jenkinsfile is not in searcher type lists
    (:language groovy :ext "gradle")
    (:language groovy :ext "groovy")
    (:language groovy :ext "jenkinsfile")
    (:language haskell :ext "hs" :agtype "haskell" :rgtype "haskell")
    (:language haskell :ext "lhs" :agtype "haskell" :rgtype "haskell")
    (:language objc :ext "m" :agtype "objc" :rgtype "objc")
    (:language csharp :ext "cs" :agtype "csharp" :rgtype "csharp")
    (:language java :ext "java" :agtype "java" :rgtype "java")
    (:language vala :ext "vala" :agtype "vala" :rgtype "vala")
    (:language vala :ext "vapi" :agtype "vala" :rgtype "vala")
    (:language julia :ext "jl" :agtype "julia" :rgtype "julia")
    (:language clojure :ext "clj" :agtype "clojure" :rgtype "clojure")
    (:language clojure :ext "cljc" :agtype "clojure" :rgtype "clojure")
    (:language clojure :ext "cljs" :agtype "clojure" :rgtype "clojure")
    (:language clojure :ext "cljx" :agtype "clojure" :rgtype "clojure")
    (:language coffeescript :ext "coffee" :agtype "coffee" :rgtype "coffeescript")
    (:language faust :ext "dsp")
    (:language faust :ext "lib")
    (:language fortran :ext "F" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f77" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f90" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f95" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "F77" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "F90" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "F95" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f03" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "for" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "ftn" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "fpp" :agtype "fortran" :rgtype "fortran")
    (:language go :ext "go" :agtype "go" :rgtype "go")
    (:language javascript :ext "js" :agtype "js" :rgtype "js")
    (:language javascript :ext "jsx" :agtype "js" :rgtype "js")
    (:language javascript :ext "vue" :agtype "js" :rgtype "js")
    (:language javascript :ext "html" :agtype "html" :rgtype "html")
    (:language javascript :ext "css" :agtype "css" :rgtype "css")
    (:language typescript :ext "ts" :agtype "ts" :rgtype "ts")
    (:language typescript :ext "tsx" :agtype "ts" :rgtype "ts")
    (:language typescript :ext "vue" :agtype "ts" :rgtype "ts")
    (:language dart :ext "dart":rgtype "dart")
    (:language lua :ext "lua" :agtype "lua" :rgtype "lua")
    ;; the extension "m" is also used by obj-c so must use matlab-mode
    ;; since obj-c will win by file extension, but here for searcher types
    (:language matlab :ext "m" :agtype "matlab" :rgtype "matlab")
    (:language nim :ext "nim" :agtype "nim" :rgtype "nim")
    (:language nix :ext "nix" :agtype "nix" :rgtype "nix")
    (:language org :ext "org" :rgtype "org")
    (:language perl :ext "pl" :agtype "perl" :rgtype "perl")
    (:language perl :ext "pm" :agtype "perl" :rgtype "perl")
    (:language perl :ext "pm6" :agtype "perl")
    (:language perl :ext "perl" :rgtype "perl")
    (:language perl :ext "plh" :rgtype "perl")
    (:language perl :ext "plx" :rgtype "perl")
    (:language perl :ext "pod" :agtype "perl" :rgtype "pod")
    (:language perl :ext "t" :agtype "perl")
    (:language php :ext "php" :agtype "php" :rgtype "php")
    (:language php :ext "php3" :agtype "php" :rgtype "php")
    (:language php :ext "php4" :agtype "php" :rgtype "php")
    (:language php :ext "php5" :agtype "php" :rgtype "php")
    (:language php :ext "phtml" :agtype "php" :rgtype "php")
    (:language php :ext "inc" :agtype "php")
    (:language python :ext "py" :agtype "python" :rgtype "py")
    (:language r :ext "R" :agtype "r" :rgtype "r")
    (:language r :ext "r" :agtype "r" :rgtype "r")
    (:language r :ext "Rmd" :agtype "r" :rgtype "r")
    (:language r :ext "Rnw" :agtype "r" :rgtype "r")
    (:language r :ext "Rtex" :agtype "r")
    (:language r :ext "Rrst" :agtype "r")
    (:language racket :ext "rkt" :agtype "racket" :rgtype "lisp")
    (:language crystal :ext "cr" :agtype "crystal" :rgtype "crystal")
    (:language crystal :ext "ecr" :agtype "crystal")
    (:language ruby :ext "rb" :agtype "ruby" :rgtype "ruby")
    (:language ruby :ext "erb" :agtype "ruby")
    (:language ruby :ext "haml" :agtype "ruby")
    (:language ruby :ext "rake" :agtype "ruby")
    (:language ruby :ext "slim" :agtype "ruby")
    (:language rust :ext "rs" :agtype "rust" :rgtype "rust")
    (:language scad :ext "scad")
    (:language scala :ext "scala" :agtype "scala" :rgtype "scala")
    (:language scheme :ext "scm" :agtype "scheme" :rgtype "lisp")
    (:language scheme :ext "ss" :agtype "scheme" :rgtype "lisp")
    (:language scheme :ext "sld" :agtype "scheme" :rgtype "lisp")
    (:language shell :ext "sh")
    (:language shell :ext "bash")
    (:language shell :ext "csh")
    (:language shell :ext "ksh")
    (:language shell :ext "tcsh")
    (:language sml :ext "sml" :agtype "sml" :rgtype "sml")
    (:language sql :ext "sql" :agtype "sql" :rgtype "sql")
    (:language swift :ext "swift" :rgtype "swift")
    (:language tex :ext "tex" :agtype "tex" :rgtype "tex")
    (:language elixir :ext "ex" :agtype "elixir" :rgtype "elixir")
    (:language elixir :ext "exs" :agtype "elixir" :rgtype "elixir")
    (:language elixir :ext "eex" :agtype "elixir" :rgtype "elixir")
    (:language erlang :ext "erl" :agtype "erlang" :rgtype "erlang")
    (:language systemverilog :ext "sv" :agtype "verilog" :rgtype "verilog")
    (:language systemverilog :ext "svh" :agtype "verilog" :rgtype "verilog")
    (:language vhdl :ext "vhd" :agtype "vhdl" :rgtype "vhdl")
    (:language vhdl :ext "vhdl" :agtype "vhdl" :rgtype "vhdl")
    (:language scss :ext "scss" :agtype "css" :rgtype "css")
    (:language pascal :ext "pas" :agtype "delphi")
    (:language pascal :ext "dpr" :agtype "delphi")
    (:language pascal :ext "int" :agtype "delphi")
    (:language pascal :ext "dfm" :agtype "delphi")
    (:language fsharp :ext "fs" :agtype "fsharp")
    (:language fsharp :ext "fsi" :agtype "fsharp")
    (:language fsharp :ext "fsx" :agtype "fsharp")
    (:language kotlin :ext "kt" :agtype "kotlin" :rgtype "kotlin")
    (:language kotlin :ext "kts" :agtype "kotlin" :rgtype "kotlin")
    (:language protobuf :ext "proto" :agtype "proto" :rgtype "protobuf")
    (:language hcl :ext "tf" :agtype "terraform" :rgtype "tf")
    (:language hcl :ext "tfvars" :agtype "terraform"))

  "Mapping of programming language(s) to file extensions."
  :type '(repeat (plist :options ((:language string :tag "Language")
                                  (:ext (string :tag "Extension"))
                                  (:agtype (string :tag "Ag type"))
                                  (:rgtype (string :tag "Ripgrep type"))))))

(defcustom dumb-jump-language-contexts
  '((:language javascript :type function :right "^(")
    (:language javascript :type variable :left "($")
    (:language javascript :type variable :right "^)" :left "($")
    (:language javascript :type variable :right "^\\.")
    (:language javascript :type variable :right "^;")
    (:language typescript :type function :right "^(")
    (:language perl :type function :right "^(")
    (:language php :type function :right "^(")
    (:language php :type class :left "new\s+")
    (:language elisp :type function :left "($")
    (:language elisp :type variable :right "^)")
    (:language scheme :type function :left "($")
    (:language scheme :type variable :right "^)"))
  "List of under points contexts for each language.
This helps limit the number of regular expressions we use
if we know that if there's a '(' immediately to the right of
a symbol then it's probably a function call"
  :type '(repeat (plist :options ((:language (symbol :tag Language))
                                  (:type (choice (const function)
                                                 (const variable)))
                                  (:left (choice (const :tag Anything nil)
                                                 (string :tag Regular expression)))
                                  (:right (choice (const :tag Anything nil)
                                                  (string :tag Regular expression)))))))

(defcustom dumb-jump-project-denoters
  '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "PkgInfo" "-pkg.el")
  "Files and directories that signify a directory is a project root."
  :type '(repeat (string :tag "Name")))

(defcustom dumb-jump-default-project "~"
  "The default project to search within if a project root is not found."
  :type 'string)

(defcustom dumb-jump-project nil
  "The project to search within if normal denoters will not work.
This should only be needed in the rarest of cases."
  :type 'string)

(defcustom dumb-jump-after-jump-hook nil
  "Hooks called after jumping."
  :type 'hook)

(defcustom dumb-jump-aggressive nil
  "If non-nil, jump aggressively with the possibility of a false positive.
If `nil` always show list of more than 1 match."
  :type 'boolean)

(defcustom dumb-jump-debug nil
  "If nil-nil, will print helpful debug information."
  :type 'boolean)

(defcustom dumb-jump-confirm-jump-to-modified-file t
  "If nil-nil, confirm before jumping to a modified file.
This may lead to an inaccurate jump. Otherwise, jump without
confirmation but print a warning."
  :type 'boolean)

(defcustom dumb-jump-comments-alist
  '((c++ . "//")
    (elisp . ";")
    (commonlisp . ";")
    (javascript . "//")
    (typescript . "//")
    (dart . "//")
    (haskell . "--")
    (lua . "--")
    (rust . "//")
    (julia . "#")
    (objc . "//")
    (csharp . "//")
    (java . "//")
    (clojure . ";")
    (coffeescript . "#")
    (faust . "//")
    (fortran . "!")
    (go . "//")
    (perl . "#")
    (php . "//")
    (python . "#")
    (matlab . "%")
    (r . "#")
    (racket . ";")
    (ruby . "#")
    (crystal . "#")
    (nim . "#")
    (nix . "#")
    (scala . "//")
    (scheme . ";")
    (shell . "#")
    (swift . "//")
    (elixir . "#")
    (erlang . "%")
    (tex . "%")
    (systemverilog . "//")
    (vhdl . "--")
    (scss . "//")
    (pascal . "//")
    (protobuf . "//")
    (hcl . "#"))
  "List of one-line comments organized by language."
  :type '(alist :key-type symbol :value-type string))

(defcustom dumb-jump-async-p t
  "If non-nil, queries will be executed asynchronously."
  :type 'boolean)


;;; Searcher Baseclass and Methods

(defclass dumb-jump-searcher ()
  ((search :initarg :search :type string)
   (root :initarg :root :type string)
   (current-line :initform (line-number-at-pos)
                 :type integer)
   (current-file :initform
                 (if (buffer-file-name)
                     (expand-file-name (buffer-file-name))
                   (buffer-file-name))
                 :type string)
   (right-context :initform nil :type (or null string))
   (left-context :initform nil :type (or null string))
   (comment-syntax :type string)
   (marker :initform (point-marker))
   (language :type symbol)
   (excludes :type (list-of string))
   (includes :type (list-of string))
   (results :type (list-of dumb-jump-target))
   (regexps :type list)
   (symbol :type symbol :allocation :class)
   (options :initform nil :type (list-of symbol)))
  "Abstract base-class for all searchers."
  :abstract t)

(cl-defgeneric dumb-jump-check-usable (searcher)
  "Check if searcher is installed and usable.")

(cl-defgeneric dumb-jump-parse-response (searcher)
  "Parse results of query.
The result must be a list of `dumb-jump-target' objects."
  (cl-loop initially (goto-char (point-min))
     until (eobp)
     when (dumb-jump-parse-response-line searcher)
     collect it
     do (forward-line)))

(cl-defgeneric dumb-jump-generate-command (searcher)
  "Generate command to be executed.
The command must be a list of arguments, starting with the
binary name.")

(cl-defmethod dumb-jump-generate-command :before ((searcher dumb-jump-searcher))
  (with-slots (language symbol (right right-context) (left left-context))
      searcher
    (unless (slot-boundp searcher 'regexps)
      (let ((query `((:language ,language)
                     (:supports ,symbol memq))))
        ;; check if query should be limited by a type of rule (function,
        ;; variable, class, ...).
        (cond (dumb-jump-functions-only
               (push '(:type function) query))
              ((not dumb-jump-ignore-context)
               (let ((res (dumb-jump-table-query-1
                           dumb-jump-language-contexts
                           `((:language ,language)
                             (:right ,right string-match-p)
                             (:left ,left string-match-p))
                           :type))
                     type)
                 ;; only set a type, if the query is unambiguous. this is
                 ;; tested by checking if removing all instances of the first
                 ;; element of the list, results in a empty list.
                 (when (and (not (delq (setq type (car res)) res))
                            type)
                   (push (list :type type) query)))))
        (oset searcher regexps
              (or (dumb-jump-table-query-1 dumb-jump-find-rules
                                           query :regex)
                  (and dumb-jump-fallback-search
                       (list dumb-jump-fallback-regex))))))))

(cl-defgeneric dumb-jump-populate-regexp (searcher regexp)
  "Generate a regular expression for SEARCHER from REGEXP.
REGEXP is a list of strings and symbols that will be processed
and then concatenated into a single string."
  (cl-sublis `((term . ,(oref searcher search))
               (word-boundary . "($|[^a-zA-Z0-9\\?\\*-])")
               (space . "[[:space:]]"))
             regexp))

(cl-defmethod dumb-jump-populate-regexp :around ((searcher dumb-jump-searcher) regexp)
  (cl-assert (listp regexp))
  (let ((parts (cl-call-next-method searcher regexp)))
    (cl-assert (cl-every #'stringp parts))
    (apply #'concat parts)))


;;; Silver Searcher (ag) Implementation

(defclass dumb-jump-ag (dumb-jump-searcher)
  ((found-ag-p :type boolean :allocation :class)
   (ag-query :initform nil :type (or null string))
   (symbol :initform 'ag)))

(cl-defmethod dumb-jump-check-usable :before ((searcher dumb-jump-ag))
  "Check if ag is installed."
  (with-temp-buffer
    (shell-command (concat dumb-jump-ag-cmd " --version") t)
    (oset searcher found-ag-p (and (search-forward "ag version" nil t) t))))

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-ag))
  "Check if ag is installed."
  (oref searcher found-ag-p))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-ag))
  "Generate command for ag."
  (let ((args dumb-jump-ag-search-args)
        (query (dumb-jump-table-query
                dumb-jump-language-file-exts
                `((:language ,(oref searcher language)))
                :agtype :ext)))
    (push (mapconcat (lambda (regexp)
                       (dumb-jump-populate-regexp
                        searcher regexp))
                     (oref searcher regexps) "|")
          args)
    (dolist (exclude (oref searcher excludes))
      (push (replace-regexp-in-string
             (regexp-quote (oref searcher root))
             "" exclude)
            args)
      (push "--ignore" args))
    (if (and (not (oref searcher ag-query))
             (assq :agtype query))
        (dolist (type (delete-dups (alist-get :agtype query)))
          (push (concat "--" type) args))
      (push (or (oref searcher ag-query)
                (mapconcat (apply-partially #'concat "\\.")
                           `("(" ,@(alist-get :ext query) ")$")
                           "|"))
            args)
      (push "-G" args))
    (when (string= (file-name-extension (oref searcher current-file)) "gz")
      (push "--search-zip" args))
    (nconc (list dumb-jump-ag-cmd "--nocolor" "--nogroup")
           args (list (oref searcher root)))))

(cl-defmethod dumb-jump-populate-regexp ((searcher dumb-jump-ag) regex)
  (cl-call-next-method
   searcher
   (cl-substitute "(?![a-zA-Z0-9\\?\\*-])" 'word-boundary regex)))


;;; Ripgrep (rg) Implementation

(defclass dumb-jump-rg (dumb-jump-searcher)
  ((symbol :initform 'rg)))

(cl-defmethod dumb-jump-check-usable ((_searcher dumb-jump-rg))
  "Check if rg is installed."
  (let (ok1 ok2)
    (with-temp-buffer                   ;ensure minimal version
      (shell-command (concat dumb-jump-rg-cmd " --version") t)
      (save-match-data
        (when (search-forward-regexp "ripgrep \\([0-9]+\\.[0-9]+\\).*" nil t)
          (setq ok1 (version<= "0.10" (match-string 1))))))
    (with-temp-buffer                   ;ensure PCRE2
      (shell-command (concat dumb-jump-rg-cmd " --pcre2 -q .") t)
      (save-match-data
        (setq ok2 (not (looking-at-p "PCRE2 is not available in this build")))))
    (and ok1 ok2)))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-rg))
  (let ((args dumb-jump-rg-search-args)
        (query (dumb-jump-table-query-1
                dumb-jump-language-file-exts
                `((:language ,(oref searcher language)))
                :rgtype)))
    (push (mapconcat (lambda (regexp)
                       (dumb-jump-populate-regexp searcher regexp))
                     (oref searcher regexps) "|")
          args)
    (dolist (exclude (oref searcher excludes))
      (push (concat "!" (replace-regexp-in-string
                         (regexp-quote (oref searcher root))
                         "" exclude))
            args)
      (push "-g" args))
    (dolist (type query)
      (push type args)
      (push "--type" args))
    (nconc (list dumb-jump-rg-cmd
                 "--color" "never" "--no-heading"
                 "--line-number" "--pcre2" "-U")
           args (list (oref searcher root)))))

(cl-defmethod dumb-jump-populate-regexp ((searcher dumb-jump-rg) regex)
  (cl-call-next-method
   searcher
   (cons (replace-regexp-in-string "\\`-" "[-]" (car regex))
                             (cdr regex))))


;;; Git-Grep Implementation

(defclass dumb-jump-git-grep (dumb-jump-searcher)
  ((found-git-grep-p :type boolean :allocation :class)
   (symbol :initform 'git-grep)))

(cl-defmethod dumb-jump-check-usable :before ((searcher dumb-jump-git-grep))
  "Check if git grep is installed."
  (with-temp-buffer
    (shell-command (concat dumb-jump-git-cmd " grep") t)
    (oset searcher found-git-grep-p
          (and (search-forward "fatal: no pattern given" nil t) t))))

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-git-grep))
  "Check if git grep is installed."
  (oref searcher found-git-grep-p))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-git-grep))
  (let ((args dumb-jump-git-grep-search-args)
        (exts (dumb-jump-table-query-1
               dumb-jump-language-file-exts
               `((:language ,(oref searcher language)))
               :ext)))
    (dolist (exclude (oref searcher excludes))
      (push (concat ":(exclude)" exclude)
            args))
    (dolist (ext exts)
      (push (format "%s/*.%s" (oref searcher root) ext)
            args))
    (push "--" args)
    (dolist (regexp (oref searcher regexps))
      (push (dumb-jump-populate-regexp searcher regexp)
            args)
      (push "-e" args))
    (when dumb-jump-git-grep-search-untracked
      (push "--untracked" args))
    (nconc (list dumb-jump-git-cmd "--no-pager" "grep"
                 "--full-name" "--color=never" "-nEI")
           args)))


;;; Git-Grep+Silver Searcher Implementation

(defclass dumb-jump-git-grep+ag (dumb-jump-ag dumb-jump-git-grep)
  ;; no symbol extra, because the rules are the same as for ag
  ())

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-git-grep+ag))
  "Check if both ag and git grep is installed."
  (and (oref searcher found-git-grep-p)
       (oref searcher found-ag-p)))

(cl-defmethod dumb-jump-generate-command :before ((searcher dumb-jump-git-grep+ag))
  (with-temp-buffer
    (let ((default-directory (oref searcher root)))
      (call-process dumb-jump-git-cmd
                    nil t nil
                    "grep" "--full-name" "-F" "-c"
                    (oref searcher search) (oref searcher root)))
    (goto-char (point-min))
    (let (files)
      (while (not (eobp))
        (when (looking-at (rx bos (group (*? nonl)) ":"))
          ;; FIXME: the files may contain symbols that could be
          ;; confused for parts of a regular expression. this should
          ;; be escaped
          (push (file-relative-name
                 (expand-file-name (match-string 1)
                                   (oref searcher root))
                 (oref searcher root))
                files)))
      (oset searcher ag-query (mapconcat #'identity
                                         `("(" ,@files ")")
                                         "|")))))


;;; General Grep Implementation

(defclass dumb-jump-grep (dumb-jump-searcher)
  ((version :initform nil :allocation :class)
   (symbol :initform 'grep)))

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-grep))
  "Check if grep is installed, and determine it's variant."
  (with-temp-buffer
    (shell-command (concat dumb-jump-grep-cmd " --version") t)
    (cond ((search-forward "GNU grep" nil t)
           (oset searcher version 'gnu))
          ((search-forward-regexp "[0-9]+\\.[0-9]+" nil t)
           (oset searcher version 'bsd))
          (t nil))))

(cl-defmethod dumb-jump-parse-response ((searcher dumb-jump-grep))
  "Parse response into `dumb-jump-target' structs.
Warnings produced by grep are ignored."
  (cl-loop initially (goto-char (point-min))
     ;; FIXME: fails is locale is not english
     ;; FIXME: fails if "No such file or" is part of the line
     until (eobp)
     unless (search-forward-regexp "^grep:\\|No such file or" (line-end-position) t)
     when (dumb-jump-parse-response-line searcher)
     collect it
     do (forward-line)))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-grep))
  (let ((args)
        (query (dumb-jump-table-query-1 dumb-jump-language-file-exts
                                        `((:language ,(oref searcher language)))
                                        :ext)))
    (dolist (regexp (oref searcher regexps))
      (push (dumb-jump-populate-regexp searcher regexp)
            args)
      (push "-e" args))
    (dolist (exclude (oref searcher excludes))
      (push (format "--exclude-dir=%s" exclude) args))
    (dolist (ext query)
      (push (format "--include=*.%s" ext) args))
    (nconc (list (if (string= (file-name-extension (oref searcher current-file)) "gz")
                     dumb-jump-zgrep-cmd dumb-jump-grep-cmd))
           dumb-jump-gnu-grep-args args (list (oref searcher root)))))


;;; GNU Grep Implementation

(defclass dumb-jump-gnu-grep (dumb-jump-grep)
  ((version :initform 'gnu)
   (symbol :initform 'gnu-grep)))

(cl-defmethod dumb-jump-check-usable :after ((searcher dumb-jump-gnu-grep))
  "Check if GNU grep is installed"
  (eq (oref searcher version) 'gnu))

(cl-defmethod dumb-jump-populate-regexp ((searcher dumb-jump-gnu-grep) regex)
  (mapcar (lambda (part)
            (if (stringp part)
                (replace-regexp-in-string "\\\\s" "[[:space:]]" part)
              part))
          (cl-call-next-method searcher regex)))


;;; Utility Functions

(defun dumb-jump-search-dwim ()
  "Attempt to guess what to search."
  (cond ((region-active-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((version< emacs-version "24.4")
         (thing-at-point 'symbol))
        (t (thing-at-point 'symbol t))))

(defun dumb-jump-git-root-p (dir)
  "Check if DIR is a git root directory."
  (file-directory-p (expand-file-name ".git" dir)))

(defun dumb-jump-get-project-root (&optional filepath)
  "Attempt to find project root directory.
Keep looking at the parent dir of FILEPATH until a denoter
file/dir is found."
  (let* ((filepath (or filepath
                       (buffer-file-name)
                       default-directory))
         (name (or dumb-jump-project
                   (locate-dominating-file filepath #'dumb-jump-get-config)
                   dumb-jump-default-project)))
    (file-name-as-directory (expand-file-name name))))

(defun dumb-jump-get-language-from-mode ()
  "Extract the language from the 'major-mode' name."
  (cl-case major-mode
    (emacs-lisp-mode 'elisp)
    (sh 'shell)
    (cperl-mode 'perl)
    (matlab-mode 'matlab)
    (octave-mode 'matlab)))

(defsubst dumb-jump-get-language-by-filename (&optional file)
  "Get the programming language from the FILE."
  (let* ((file (or file
                   (and (buffer-file-name)
                        (file-name-nondirectory (buffer-file-name)))
                   (buffer-name)))
         (filename (if (string= (file-name-extension file) "gz")
                       (file-name-sans-extension file)
                     file))
         (result (cl-find (file-name-extension filename)
                          dumb-jump-language-file-exts
                          :key (lambda (x) (plist-get x :ext))
                          :test #'string=)))
    (plist-get result :language)))

(defsubst dumb-jump-get-mode-base-name ()
  "Get the base name of the mode."
  (let ((name (symbol-name major-mode)))
    (and (string-suffix-p "-mode" name)
         (substring name 0 -5))))

(defsubst dumb-jump-get-language ()
  "Get language from FILE extension and then fallback to using `major-mode' name."
  (or (dumb-jump-get-language-from-mode)
      (dumb-jump-get-language-by-filename)
      (dumb-jump-get-mode-base-name)
      (error "Could not find rules for current buffer")))

(defconst dumb-jump-process-regexp-alist
  '((clojure . "\\`.*?/\\(.+\\)$")
    (ruby . "\\`\\(?:.*::\\|:\\)\\(.*?\\)\\'")
    (crystal . "\\`:\\(.+\\)")
    (systemverilog . "\\``\\(.+\\)"))
  "Alist of regular expressions to process symbols.
Each regular expression must has a group number 1, that matches
the actual symbol name.")

(defun dumb-jump-process-symbol (symbol lang)
  "Process SYMBOL of language LANG at point.
For instance, clojure needs namespace part removed."
  (save-match-data
    (let ((regexp (cdr (assoc lang dumb-jump-process-regexp-alist))))
      (if (and regexp (string-match regexp symbol))
          (match-string 1 symbol)
        symbol))))

(defun dumb-jump-get-lang-by-shell-contents (&optional buffer)
  "Return languages in BUFFER by checking if file extension is mentioned."
  (with-current-buffer (or buffer (current-buffer))
    (catch 'found
      (save-excursion
        (dolist (lang dumb-jump-language-file-exts)
          (goto-char (point-max))
          (when (search-backward-regexp
                 (concat "\\." (plist-get lang :ext) "\\b")
                 nil t)
            (throw 'found (plist-get lang :language))))))))

(defsubst dumb-jump-check-queries (queries ent)
  "Check if all entry ENT satisfies all QUERIES.
See `dumb-jump-table-query' for more details on the structure of
QUERIES."
  (catch 'fail
    (dolist (query queries t)
      (unless (and (nth 1 query)
                   (plist-get ent (nth 0 query))
                   (funcall (or (nth 2 query) #'eq)
                            (nth 1 query)
                            (plist-get ent (nth 0 query))))
        (throw 'fail nil)))))

(defun dumb-jump-table-query (table queries &rest fields)
  "Query TABLE using QUERIES.
TABLE must be a list of plists. QUERIES is a list of lists, where
the first element specifies a field to be tested, the second
value is the value to be tested and an optional third element
defines a relation. If the relation is nil, default to `eq'. The
remaining arguments FIELDS list all FIELDS that the query should
return. The result is an alist, whose keys are the values of
FIELDS, while the values are the non-nil results of the query.

For example

    (dumb-jump-table-query dumb-jump-find-rules
                           '((:language c++)
                             (:supports git-grep memq))
                           :regex :type)

will return all :regex and :type fields from
`dumb-jump-find-rules', where the language of the entry is c++
and the :supports field has git-grep as a member. It should look
something like this:

    ((:regex ...)
     (:type ...))"
  (let ((results (mapcar #'list fields)))
    (dolist (ent table)
      (when (dumb-jump-check-queries queries ent)
        (dolist (field fields)
          (when (plist-get ent field)
            (push (plist-get ent field)
                  (alist-get field results))))))
    results))

(defun dumb-jump-table-query-1 (table queries field)
  "Shorthand variant of `dumb-jump-table-query'.
Useful if the result should only contain one FIELD. See
`dumb-jump-table-query', for more details on TABLE and QUERIES."
  (cdar (dumb-jump-table-query table queries field)))


;;; Configurations

(defun dumb-jump-get-config (dir)
  "Attempt to find configuration file in DIR.
Checks if any file name from `dumb-jump-project-denoters' can be
found in the current directory, except if there is a file named
\".dumbjumpignore\". If nothing was found, nil is returned."
  (unless (file-exists-p (expand-file-name ".dumbjumpignore" dir))
    (cl-loop for denoter in dumb-jump-project-denoters
       when (file-exists-p (expand-file-name denoter dir))
       return it)))

(defun dumb-jump-load-config (searcher &optional config-file)
  "Load CONFIG-FILE data into SEARCHER.
In case CONFIG-FILE is nil, default to \".dumbjump\" in the root
project directory."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (let ((lang (if (memq major-mode '(shell-mode eshell-mode))
                  (dumb-jump-get-lang-by-shell-contents)
                (dumb-jump-get-language)))
        include exclude
        (file (expand-file-name (or config-file ".dumbjump")
                                (oref searcher root))))
    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file)
        (let* ((root (oref searcher root))
               (local-root (if (file-remote-p root)
                               (tramp-file-name-localname
                                (tramp-dissect-file-name
                                 root))
                             root)))
          (while (not (eobp))
            (cond ((looking-at "^language \\\(.+\\\)")
                   (setq lang (intern (match-string 1))))
                  ((looking-at "^\\+\\(.+\\)")
                   (push (expand-file-name (match-string 1) local-root)
                         include))
                  ((looking-at "^-/?\\(.+\\)")
                   (push (expand-file-name (match-string 1) local-root)
                         exclude)))
            (forward-line)))))
    (oset searcher language lang)
    (oset searcher excludes (delete-dups exclude))
    (oset searcher includes (delete-dups (cons (oref searcher root)
                                               include)))
    (let ((syntax (assq lang dumb-jump-comments-alist)))
      (when syntax
        (oset searcher comment-syntax (regexp-quote (cdr syntax)))))
    (let* ((symbol (oref searcher search))
           (search (dumb-jump-process-symbol symbol lang)))
      (oset searcher search search))))


;;; Query Constructor

(defun dumb-jump-load-context (searcher)
  "Set the context values for SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (save-match-data
    (with-slots (search) searcher
      (when (looking-at (concat "^\\(.*\\)"
                                (regexp-quote search)
                                "\\(.*?\\)$"))
        (unless (string= (match-string 1) "")
          (oset searcher left-context (match-string 1)))
        (unless (string= (match-string 2) "")
          (oset searcher right-context (match-string 2)))))))

(defun dumb-jump-pick-searcher (prompt)
  "Initialise and return a searcher.
If PROMPT is non-nil, set PROMPT as search term. Otherwise try to
locate the symbol at point."
  (let ((try '(dumb-jump-ag                 ; ordered by preference
               dumb-jump-rg
               dumb-jump-gnu-grep
               dumb-jump-grep))
        (root (dumb-jump-get-project-root))
        (search (or prompt (dumb-jump-search-dwim)
                    (user-error "No symbol at point"))))
    (when (dumb-jump-git-root-p root)
      (push 'dumb-jump-git-grep try))
    (when dumb-jump-prefer-searcher
      (push dumb-jump-prefer-searcher try))
    (when dumb-jump-force-searcher
      (setq try (list dumb-jump-force-searcher)))
    (catch 'found
      (let (searcher)
        (dolist (class try)
          (setq searcher (make-instance class
                                        :search search
                                        :root root))
          (when (dumb-jump-check-usable searcher)
            (dumb-jump-load-config searcher)
            (unless prompt
              (dumb-jump-load-context searcher))
            (throw 'found searcher)))
        (error "No usable search tool found")))))

(defun dumb-jump-sentinel (proc event)
  "Sentinel to PROC output when process finishes.
Any value besides \"finished\" for EVENT is an error."
  (let ((inhibit-message (not dumb-jump-debug))
        (searcher (process-get proc :dumb-jump-searcher)))
    (unwind-protect
         (cond ((and dumb-jump-fallback-search
                     (with-current-buffer (process-buffer proc)
                       (= (point-min) (point-max)))
                     (not (equal (list dumb-jump-fallback-regex)
                                 (oref searcher regexps))))
                (oset searcher regexps (list dumb-jump-fallback-regex))
                (dumb-jump-query searcher))
               ((string= event "finished\n")
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-min))
                  (dumb-jump-process-results searcher)))
               (t (error "Unexpected event %s"
                         (replace-regexp-in-string "\n$" "" event))))
      (when (and (buffer-live-p (process-buffer proc))
                 (not dumb-jump-debug))
        (kill-buffer (process-buffer proc))))))

(defun dumb-jump-query (searcher)
  "Run query using SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (let* ((buf (generate-new-buffer " *dumb-jump-query*"))
         (cmd (dumb-jump-generate-command searcher))
         (process-environment (cons "TERM" process-environment))
         (default-directory (oref searcher root)))
    (cl-assert (listp cmd))
    (cl-assert (cl-every #'stringp cmd))
    (message "Query command: %S" cmd)
    (if dumb-jump-async-p
        (let ((proc (apply #'start-file-process "dumb-jump" buf cmd)))
          (process-put proc :dumb-jump-searcher searcher)
          (set-process-sentinel proc #'dumb-jump-sentinel))
      (with-current-buffer buf
        (apply #'process-file (car cmd) nil t nil (cdr cmd))
        (goto-char (point-min))
        (prog1 (dumb-jump-process-results searcher)
          (unless dumb-jump-debug
            (kill-buffer)))))))


;;; Result Handling

(cl-defstruct dumb-jump-target path line column context)

(defun dumb-jump-parse-response-line (searcher)
  "Return a `dumb-jump-target' or nil, if line has no data.
SEARCHER is an instance of `dumb-jump-seacher'.
The match data is modified by this function."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (cl-assert (= (line-beginning-position) (point)))
  (with-slots (root search current-file current-line comment-syntax)
      searcher
    (and (looking-at (concat "^\\(.*\\)\\(?:^\\|:\\)\\([0-9]+\\):\\(\\(.*?\\)\\("
                             (regexp-quote search) "\\).*\\)[[:space:]]*$"))
         ;; if the regular expression was accepted, we have found a
         ;; file-path, a line number and the "context", ie. the entire
         ;; line that was matched.
         (let ((path (expand-file-name
                      (or (match-string 1) current-file)
                      root))
               (line (string-to-number (match-string 2))))
           (and
            ;; check if variable mention is not in a comment
            (if (slot-boundp searcher 'comment-syntax)
                (not (string-match-p comment-syntax (match-string 4)))
              t)
            ;; ignore the result, if it is on the same place where the
            ;; started the query.
            (not (and (file-equal-p path current-file)
                      (= line current-line)))
            ;; target passed all heuristics. return it to the parser.
            ;; TODO: add column
            (make-dumb-jump-target :path path :line line
                                   :column (- (match-beginning 5)
                                              (match-beginning 3))
                                   :context (match-string 3)))))))

(defvar dumb-jump-return-results nil
  "If non-nil, make `dumb-jump-process-results' return results.
Has to be used with a nil value for `dumb-jump-async-p'.")

(defvar dumb-jump-order-results t
  "If non-nil, order results by distance to point.")

(defun dumb-jump-process-results (searcher)
  "Process results in buffer using SEARCHER.
Depending on the dynamic context, this means sorting and
filtering the raw data, and then passing it on to
`dumb-jump-handle-results'."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (let ((parsed (save-match-data
                  (delete-dups
                   (dumb-jump-parse-response searcher)))))
    (when (and dumb-jump-order-results
               (< 1 (length parsed)))
      (cl-callf2 dumb-jump-sort-results searcher parsed))
    (dumb-jump-handle-results searcher parsed)))

(defun dumb-jump-sort-results (searcher parsed)
  "Sort PARSED using SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (cl-assert (cl-every #'dumb-jump-target-p parsed))
  ;; NB: `sort' is a stable sorting algorithm
  (cl-labels ((order-by-lines (right left)
                (and (file-equal-p (dumb-jump-target-path right)
                                   (dumb-jump-target-path left))
                     (< (dumb-jump-target-line right)
                        (dumb-jump-target-line left))))
              (prefer-closer-line (right left)
                (and (file-equal-p (dumb-jump-target-path left)
                                   (oref searcher current-file))
                     (file-equal-p (dumb-jump-target-path right)
                                   (oref searcher current-file))
                     (< (abs (- (oref searcher current-line)
                                (dumb-jump-target-line right)))
                        (abs (- (oref searcher current-line)
                                (dumb-jump-target-line left))))))
              (relative-path (target)
                (let* ((path (dumb-jump-target-path target))
                       (here (oref searcher current-file))
                       (base (file-name-directory here)))
                  (file-relative-name path base)))
              (file-distance (file)
                (let ((count 0))
                  (while (setq file (file-name-directory file))
	                (setq file (substring file 0 -1)
                          count (1+ count)))
                  count))
              (prefer-closer-file (right left)
                (< (file-distance (relative-path right))
                   (file-distance (relative-path left))))
              (prefer-external (right left)
                (and (file-equal-p (dumb-jump-target-path left)
                                   (oref searcher current-file))
                     (not (file-equal-p (dumb-jump-target-path right)
                                        (oref searcher current-file)))))
              (prefer-local (right left)
                (prefer-external left right)))
    ;; by default, all matches within a file should be sorted by their
    ;; line number.
    (cl-callf sort parsed #'order-by-lines)
    ;; prefer matches closer to the line where `dumb-jump-go' was
    ;; invoked, to those further away.
    (cl-callf sort parsed #'prefer-closer-line)
    ;; prefer files closer to the file system where `dumb-jump-go' was
    ;; invoked, to those further away.
    (cl-callf sort parsed #'prefer-closer-file)
    ;; in case requested, external matches (ie. a match outside of the
    ;; current file) should be listed before file-local matches.
    (if (memq 'external (oref searcher options))
        (cl-callf sort parsed #'prefer-external)
      (cl-callf sort parsed #'prefer-local))
    ;; the "aggressive" mode tells us to only return one result.
    ;; either way, the result should be converted back from a vector
    ;; to a list.
    (if dumb-jump-aggressive
        (list (car parsed))
      parsed)))

(defun dumb-jump-handle-results (searcher parsed)
  "Handle PARSED using SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (cond (dumb-jump-return-results parsed)
        ((cdr parsed)                   ; (> (length parsed) 1)
         (dumb-jump-open-target searcher (dumb-jump-choose-target parsed)))
        ((car parsed)                   ; (= (length parsed) 1)
         (dumb-jump-open-target searcher (car parsed)))
        (t (error "No results"))))      ; (= (length parsed) 0)

(defun dumb-jump-choose-target (targets)
  "Prompt user to choose a member of TARGETS."
  (cl-assert (cl-every #'dumb-jump-target-p targets))
  (let (options)
    (dolist (target targets)
      (push (cons (format "%s:%s: %s"
                          (file-name-nondirectory
                           (dumb-jump-target-path target))
                          (dumb-jump-target-line target)
                          (dumb-jump-target-context target))
                  target)
            options))
    (let ((choice (completing-read "Jump to: " options nil t)))
      (cdr (or (assoc choice options)
               (error "No target chosen"))))))

(defun dumb-jump-open-target (searcher target)
  "Open TARGET using SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (cl-assert (dumb-jump-target-p target))
  (unless (let* ((path (dumb-jump-target-path target))
                 (buf (find-buffer-visiting path)))
            (and dumb-jump-confirm-jump-to-modified-file
                 buf (buffer-modified-p buf)
                 (not (yes-or-no-p (concat path " has been\
 modified, so the location may be wrong. Continue? ")))))
    (if (fboundp 'xref-push-marker-stack)
        (xref-push-marker-stack (oref searcher marker))
      (with-no-warnings
        (ring-insert find-tag-marker-ring (oref searcher marker))))
    (let* ((path (dumb-jump-target-path target))
           (buf (find-buffer-visiting path))
           (win (get-buffer-window buf 'visible)))
      ;; TODO: re-implement tool-tip support using popup and posframe
      (cond ((and dumb-jump-use-visible-window buf win)
             (select-window win))
            (buf
             (select-window (display-buffer buf)))
            ((memq 'other (oref searcher options))
             (find-file-other-window path))
            (t (find-file path)))
      (goto-char (point-min))
      (forward-line (1- (dumb-jump-target-line target)))
      (when (dumb-jump-target-column target)
        (forward-char (dumb-jump-target-column target)))
      (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
        (run-hooks 'dumb-jump-after-jump-hook)))))


;;; Interactive Commands

;;;###autoload
(defun dumb-jump-go (&optional use-tooltip prefer-external prompt)
  "Go to the function/variable declaration for thing at point.
When USE-TOOLTIP, a tooltip jump preview will show instead.
When PREFER-EXTERNAL, external matches are sorted before
current file.
If PROMPT, try to go to PROMPT instead of the symbol at point."
  (interactive "P")
  (let ((inhibit-message (not dumb-jump-debug))
        (searcher (dumb-jump-pick-searcher prompt)))
    (when use-tooltip
      ;; FIXME: not currently implemented
      (push 'toolip (oref searcher options)))
    (when prefer-external
      (push 'external (oref searcher options)))
    (push dumb-jump-window (oref searcher options))
    (dumb-jump-query searcher)))

;;;###autoload
(defun dumb-jump-quick-look ()
  "Run `dumb-jump-go' in quick look mode.
That is, show a tooltip of where it would jump instead."
  (interactive)
  (dumb-jump-go t))

;;;###autoload
(defun dumb-jump-go-other-window ()
  "Like `dumb-jump-go', but open result in other window."
  (interactive)
  (let ((dumb-jump-window 'other))
    (dumb-jump-go)))

;;;###autoload
(defun dumb-jump-go-current-window ()
  "Like `dumb-jump-go', but force opening in current window."
  (interactive)
  (let ((dumb-jump-window 'current))
    (dumb-jump-go)))

;;;###autoload
(defun dumb-jump-go-prefer-external ()
  "Run `dumb-jump-go', but prefer an external match.
This means that in case there is more than one match, those not
in the current file should be listed in front of the others in
the current file."
  (interactive)
  (dumb-jump-go nil t))

;;;###autoload
(defun dumb-jump-go-prompt ()
  "Like `dumb-jump-go', but prompt for symbol to look for."
  (interactive)
  (let* ((at-point (dumb-jump-search-dwim))
         (prompt (if at-point
                     (format "Jump to (defaults to `%s'): " at-point)
                   "Jump to: "))
         (symbol (read-from-minibuffer prompt nil nil nil nil at-point)))
    (dumb-jump-go nil nil symbol)))

;;;###autoload
(defun dumb-jump-go-prefer-external-other-window ()
  "Like `dumb-jump-go-prefer-external', but use other window."
  (interactive)
  (let ((dumb-jump-window 'other))
    (dumb-jump-go-prefer-external)))

;;;###autoload
(defun dumb-jump-back ()
  "Jump back to where the last jump was done.
Functionally equivalent to `xref-pop-marker-stack', except that the
`dumb-jump-after-jump-hook' hook is run."
  (interactive)
  (pop-tag-mark)
  (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
    (run-hooks 'dumb-jump-after-jump-hook)))


;;; Minor Mode

;;;###autoload
(defvar dumb-jump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-g") 'dumb-jump-go)
    (define-key map (kbd "C-M-b") 'dumb-jump-back)
    (define-key map (kbd "C-M-q") 'dumb-jump-quick-look)
    map))

;;;###autoload
(define-minor-mode dumb-jump-mode
    "Minor mode for jumping to variable and function definitions"
  :global t
  :keymap dumb-jump-mode-map)

(provide 'dumb-jump)

;;; dumb-jump.el ends here
