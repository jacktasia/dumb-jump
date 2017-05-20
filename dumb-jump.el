;;; dumb-jump.el --- jump to definition for multiple languages without configuration.

;; Copyright (C) 2015-2016 jack angers
;; Author: jack angers
;; Version: 0.5.0
;; Package-Requires: ((emacs "24.3") (f "0.17.3") (s "1.11.0") (dash "2.9.0") (popup "0.5.3"))
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

;; Dumb Jump is an Emacs "jump to definition" package with support for multiple programming languages that favors
;; "just working" over speed or accuracy.  This means minimal -- and ideally zero -- configuration with absolutely
;; no stored indexes (TAGS) or persistent background processes.  Dumb Jump performs best with The Silver Searcher
;; `ag` or ripgrep `rg` installed.  Dumb Jump requires at least GNU Emacs 24.3.

;;; Code:
(require 'etags)
(require 'f)
(require 's)
(require 'dash)
(require 'popup)

(defgroup dumb-jump nil
  "Easily jump to project function and variable definitions"
  :group 'tools
  :group 'convenience)

;;;###autoload
(defvar dumb-jump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-g") 'dumb-jump-go)
    (define-key map (kbd "C-M-p") 'dumb-jump-back)
    (define-key map (kbd "C-M-q") 'dumb-jump-quick-look)
    map))

(defcustom dumb-jump-window
  'current
  "Which window to use when jumping. Valid options are 'current (default) or 'other."
  :group 'dumb-jump
  :type '(choice (const :tag "Current window" current)
                 (const :tag "Other window" other)))

(defcustom dumb-jump-use-visible-window
  t
  "When true will jump in a visible window if that window already has the file open."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-selector
  'popup
  "Which selector to use when there is multiple choices.  `ivy` and `helm' are also supported."
  :group 'dumb-jump
  :type '(choice (const :tag "Popup" popup)
                 (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)))

(defcustom dumb-jump-prefer-searcher
  nil
  "The preferred searcher to use 'ag, 'rg, 'git-grep, 'gnu-grep,
or 'grep. If `nil' then the most optimal searcher will be chosen
at runtime."
  :group 'dumb-jump
  :type '(choice (const :tag "Best Available" nil)
                 (const :tag "ag" ag)
                 (const :tag "rg" rg)
                 (const :tag "grep" gnu-grep)
                 (const :tag "git grep" git-grep)))


(defcustom dumb-jump-force-searcher
  nil
  "Forcibly use searcher: 'ag, 'rg, 'git-grep, 'gnu-grep,
or 'grep. Set to `nil' to not force anything and use
`dumb-jump-prefer-searcher' or most optimal searcher."
  :group 'dumb-jump
  :type '(choice (const :tag "Best Available" nil)
                 (const :tag "ag" ag)
                 (const :tag "rg" rg)
                 (const :tag "grep" gnu-grep)
                 (const :tag "git grep" git-grep)))

(defcustom dumb-jump-grep-prefix
  "LANG=C"
  "Prefix to grep command.  Seemingly makes it faster for pure text."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-grep-cmd
  "grep"
  "The path to grep.  By default assumes it is in path."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-ag-cmd
  "ag"
  "The the path to the silver searcher.  By default assumes it is in path.  If not found fallbacks to grep."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-rg-cmd
  "rg"
  "The the path to ripgrep.  By default assumes it is in path.  If not found fallbacks to grep."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-git-grep-cmd
  "git grep"
  "The the path to git grep.  By default assumes it is in path.  If not found fallbacks to grep."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-ag-word-boundary
  "(?![\\w-])"
  "`\\b` thinks `-` is a word boundary.  When this matters use `\\j` instead and ag will use this value."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-rg-word-boundary
  "($|[^\\w-])"
  "`\\b` thinks `-` is a word boundary.  When this matters use `\\j` instead and rg will use this value."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-git-grep-word-boundary
  "($|[^\\w-])"
  "`\\b` thinks `-` is a word boundary.  When this matters use `\\j` instead and git grep will use this value."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-grep-word-boundary
  "($|[^\\w-])"
  "`\\b` thinks `-` is a word boundary.  When this matters use `\\j` instead and grep will use this value."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-fallback-regex
  "\\bJJJ\\j"
  "When dumb-jump-fallback-search is t use this regex.  Defaults to boundary search of symbol under point."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-fallback-search
  t
  "If nothing is found with normal search fallback to searching the fallback regex."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-force-grep
  nil
  "When t will use grep even if ag is available."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-zgrep-cmd
  "zgrep"
  "The path to grep to use for gzipped files.  By default assumes it is in path."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-grep-args "-REn"
  "Grep command args [R]ecursive, [E]xtended regexes, and show line [n]umbers."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-gnu-grep-args "-rEn"
  "Grep command args [r]ecursive and [E]xtended regexes, and show line [n]umbers."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-max-find-time
  2
  "Number of seconds a grep/find command can take before being warned to use ag and config."
  :group 'dumb-jump
  :type 'integer)

(defcustom dumb-jump-functions-only
  nil
  "Should we only jump to functions?"
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-quiet
  nil
  "If non-nil Dumb Jump will not log anything to *Messages*."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-ignore-context
  nil
  "If non-nil Dumb Jump will ignore the context of point when jumping."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-git-grep-search-untracked
  t
  "If non-nil Dumb Jump will also search untracked files when
using searcher git-grep."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-find-rules
  '((:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "elisp" :regex "\\\((defun|cl-defun)\\s+JJJ\\j"
           ;; \\j usage see `dumb-jump-ag-word-boundary`
           :tests ("(defun test (blah)" "(defun test\n" "(cl-defun test (blah)" "(cl-defun test\n")
           :not ("(defun test-asdf (blah)" "(defun test-blah\n" "(cl-defun test-asdf (blah)" "(cl-defun test-blah\n"))


    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
           :regex "\\\(defvar\\b\\s*JJJ\\j" :tests ("(defvar test " "(defvar test\n"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
           :regex "\\\(defcustom\\b\\s*JJJ\\j" :tests ("(defcustom test " "(defcustom test\n"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
           :regex "\\\(setq\\b\\s*JJJ\\j" :tests ("(setq test 123)") :not ("setq test-blah 123)"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
           :regex "\\\(JJJ\\s+" :tests ("(let ((test 123)))") :not ("(let ((test-2 123)))"))

    ;; variable in method signature
    (:type "variable" :supports ("ag" "rg" "git-grep") :language "elisp"
           :regex "\\((defun|cl-defun)\\s*.+\\\(?\\s*JJJ\\j\\s*\\\)?"
           :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)")
           :not ("(defun blah (test-1)" "(defun blah (test-2 blah)" "(defun (blah test-3)"))

    ;; c++
    (:type "function" :supports ("ag" "rg" "git-grep") :language "c++"
           :regex "\\bJJJ(\\s|\\))*\\((\\w|[,&*.<>]|\\s)*(\\))\\s*(const|->|\\{|$)|typedef\\s+(\\w|[(*]|\\s)+JJJ(\\)|\\s)*\\("
           :tests ("int test(){" "my_struct (*test)(int a, int b){" "auto MyClass::test ( Builder& reference, ) -> decltype( builder.func() ) {" "int test( int *random_argument) const {" "test::test() {" "typedef int (*test)(int);")
           :not ("return test();)" "int test(a, b);" "if( test() ) {" "else test();"))

    ;; (:type "variable" :supports ("grep") :language "c++"
    ;;        :regex "(\\b\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[([0-9]|\\s)*\\])*\\s*([=,){;]|:\\s*[0-9])|#define\\s+JJJ\\b"
    ;;        :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "unsigned int test:2;"))

    (:type "variable" :supports ("ag") :language "c++"
           :regex "\\b(?!(class\\b|struct\\b|return\\b|else\\b|delete\\b))(\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[(\\d|\\s)*\\])*\\s*([=,(){;]|:\\s*\\d)|#define\\s+JJJ\\b"
           :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "typedef int test;" "unsigned int test:2")
           :not ("return test;" "#define NOT test" "else test=2;"))

    (:type "type" :supports ("ag" "rg" "git-grep") :language "c++"
           :regex "\\b(class|struct|enum|union)\\b\\s*JJJ\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*JJJ\\b\\s*;"
           :tests ("typedef struct test {" "enum test {" "} test;" "union test {" "class test final: public Parent1, private Parent2{" "class test : public std::vector<int> {")
           :not("union test var;" "struct test function() {"))

    ;; clojure
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(def\\s+JJJ\\j"
           :tests ("(def test (foo)"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(defn-?\\s+JJJ\\j"
           :tests ("(defn test [foo]" "(defn- test [foo]"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(defmacro\\s+JJJ\\j"
           :tests ("(defmacro test [foo]"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(deftype\\s+JJJ\\j"
           :tests ("(deftype test [foo]"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(defmulti\\s+JJJ\\j"
           :tests ("(defmulti test fn"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(defmethod\\s+JJJ\\j"
           :tests ("(defmethod test type"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(definterface\\s+JJJ\\j"
           :tests ("(definterface test (foo)"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(defprotocol\\s+JJJ\\j"
           :tests ("(defprotocol test (foo)"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
           :regex "\\(defrecord\\s+JJJ\\j"
           :tests ("(defrecord test [foo]"))

    ;; coffeescript
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "coffeescript"
           :regex "^\\s*JJJ\\s*[=:].*[-=]>"
           :tests ("test = ()  =>" "test= =>" "test = ->" "test=()->"
                   "test : ()  =>" "test: =>" "test : ->" "test:()->")
           :not ("# test = =>" "test = 1"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "coffeescript"
           :regex "^\\s*JJJ\\s*[:=][^:=-][^>]+$"
           :tests ("test = $" "test : [" "test = {" "test = a")
           :not ("test::a" "test: =>" "test == 1" "# test = 1"))

    (:type "class" :supports ("ag" "grep" "rg" "git-grep") :language "coffeescript"
           :regex "^\\s*\\bclass\\s+JJJ"
           :tests ("class test" "class test extends")
           :not ("# class"))

    ;; obj-c
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "objc"
           :regex "\\\)\\s*JJJ(:|\\b|\\s)"
           :tests ("- (void)test" "- (void)test:(UIAlertView *)alertView")
           :not ("- (void)testnot" "- (void)testnot:(UIAlertView *)alertView"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "objc"
           :regex "\\b\\*?JJJ\\s*=[^=\\n]+"
           :tests ("NSString *test = @\"asdf\"")
           :not ("NSString *testnot = @\"asdf\"" "NSString *nottest = @\"asdf\""))

    ;; swift
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
           :regex "(let|var)\\s*JJJ\\s*(=|:)[^=:\\n]+"
           :tests ("let test = 1234" "var test = 1234" "private lazy var test: UITapGestureRecognizer") :not ("if test == 1234:"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
           :regex "func\\s*JJJ\\b\\s*\\\("
           :tests ("func test(asdf)" "func test()")
           :not ("func testnot(asdf)" "func testnot()"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
           :regex "(class|struct)\\s*JJJ\\b\\s*?"
           :tests ("class test:" "class test: UIWindow")
           :not ("class testnot:" "class testnot(object):"))

    ;; c#
    (:type "function" :supports ("ag" "rg") :language "csharp"
           :regex "^\\s*(?:[^=\\W]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                   "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
           :not ("test()" "testnot()" "blah = new test()"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "csharp"
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "csharp"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:" "public class test : IReadableChannel, I")
           :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; java (literally the same regexes as c#, but differents tests)
    (:type "function" :supports ("ag" "rg") :language "java"
           :regex "^\\s*(?:[^=\\W]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
                   "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
           :not ("test()" "testnot()" "blah = new test()"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "java"
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "java"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:" "public class test implements Something")
           :not ("class testnot:" "public class testnot implements Something"))

    ;; python
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "python"
           :regex "\\s*JJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test == 1234:"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "python"
           :regex "def\\s*JJJ\\b\\s*\\\("
           :tests ("\tdef test(asdf)" "def test()")
           :not ("\tdef testnot(asdf)" "def testnot()"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "python"
           :regex "class\\s*JJJ\\b\\s*\\\(?"
           :tests ("class test(object):" "class test:")
           :not ("class testnot:" "class testnot(object):"))

    ;; ruby
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "ruby"
           :regex "\\s*JJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234"))

    (:type "function" :supports ("ag" "rg") :language "ruby"
           :regex "\\bdef\\s+(?:\\w+(::|\\.))*JJJ\\j"
           :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
                   "def self.test()" "def MODULE::test()"))

    (:type "function" :supports ("ag" "rg") :language "ruby"
           :regex "\\bdefine(?:_singleton|_instance)?_method\\s*(?:\\(\\s*)?:JJJ\\j"
           :tests ("define_method(:test, &body)"
                   "mod.define_instance_method(:test) { body }"))

    (:type "type" :supports ("ag" "rg") :language "ruby"
           :regex "\\bclass\\s+(?:\\w*::)*JJJ\\j"
           :tests ("class test" "class Foo::test"))

    (:type "type" :supports ("ag" "rg") :language "ruby"
           :regex "\\bmodule\\s+(?:\\w*::)*JJJ\\j"
           :tests ("module test" "module Foo::test"))

    ;; scala
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "\\bval\\s*JJJ\\s*=[^=\\n]+" :tests ("val test = 1234") :not ("case test => 1234"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "\\bvar\\s*JJJ\\s*=[^=\\n]+" :tests ("var test = 1234") :not ("case test => 1234"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "\\btype\\s*JJJ\\s*=[^=\\n]+" :tests ("type test = 1234") :not ("case test => 1234"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "\\bdef\\s*JJJ\\s*\\\("
           :tests ("def test(asdf)" "def test()"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "class\\s*JJJ\\s*\\\(?"
           :tests ("class test(object)"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "trait\\s*JJJ\\s*\\\(?"
           :tests ("trait test(object)"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
           :regex "object\\s*JJJ\\s*\\\(?"
           :tests ("object test(object)"))

    ;; R
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "r"
           :regex "\\bJJJ\\s*=[^=><]" :tests ("test = 1234") :not ("if (test == 1234)"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "r"
           :regex "\\bJJJ\\s*<-\\s*function"
           :tests ("test <- function"))


    ;; perl
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "perl"
           :regex "sub\\s*JJJ\\s*\\\{"
           :tests ("sub test{" "sub test {"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "perl"
           :regex "JJJ\\s*=\\s*"
           :tests ("$test = 1234"))

    ;; shell
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "shell"
           :regex "function\\s*JJJ\\s*"
           :tests ("function test{" "function test {" "function test () {")
           :not   ("function nottest {"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "shell"
           :regex "\\bJJJ\\s*=\\s*"
           :tests ("test = 1234") :not ("blahtest = 1234"))

    ;; php
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "php"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "php"
           :regex "(\\s|->|\\$|::)JJJ\\s*=\\s*"
           :tests ("$test = 1234" "$foo->test = 1234"))

    (:type "trait" :supports ("ag" "grep" "rg" "git-grep") :language "php"
           :regex "trait\\s*JJJ\\s*\\\{"
           :tests ("trait test{" "trait test {"))

    (:type "interface" :supports ("ag" "grep" "rg" "git-grep") :language "php"
           :regex "interface\\s*JJJ\\s*\\\{"
           :tests ("interface test{" "interface test {"))

    (:type "class" :supports ("ag" "grep" "rg" "git-grep") :language "php"
           :regex "class\\s*JJJ\\s*(extends|implements|\\\{)"
           :tests ("class test{" "class test {" "class test extends foo" "class test implements foo"))

    ;; faust
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "faust"
           :regex "\\bJJJ\(\\\(.+\\\)\)*\\s*="
           :tests ("test = osc + 0.5;" "test(freq) = osc(freq) + 0.5;"))

    ;; fortran
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if (test == 1234)"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
           :regex "\\b(function|subroutine)\\s+JJJ\\b\\s*\\\("
           :tests ("function test (foo)" "integer function test(foo)" "subroutine test (foo, bar)")
           :not ("end function test" "end subroutine test"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
           :regex "^\\s*module\\s+JJJ\\s*"
           :tests ("module test")
           :not ("end module test"))

    ;; go
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "go"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test == 1234 {"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "go"
           :regex "\\s*\\bJJJ\\s*:=\\s*" :tests ("test := 1234"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "go"
           :regex "func\\s+\\\([^\\\)]*\\\)\\s+JJJ\\s*\\\("
           :tests ("func (s *blah) test(filename string) string {"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "go"
           :regex "func\\s+JJJ\\s*\\\("
           :tests ("func test(url string) (string, error)"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "go"
           :regex "type\\s+JJJ\\s+struct\\s+\\\{"
           :tests ("type test struct {"))

    ;; javascript extended
    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "(service|factory)\\\(['\"]JJJ['\"]" :tags ("angular")
           :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>" :tags ("es6")
           :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "\\bJJJ\\s*\\\([^\\\)]*\\\)\\s*[{]" :tags ("es6")
           :tests ("test(foo) {" "test (foo){" "test(foo){"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript" :tags ("es6")
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test(object) {" "class test{"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript" :tags ("es6")
           :regex "class\\s*JJJ\\s+extends"
           :tests ("class test extends Component{"))

    ;; javascript
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234" "const test = props =>") :not ("if (test === 1234)"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
           :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
                 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
           :tests ("test: function()"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    ;; haskell
    (:type "function" :supports ("ag") :language "haskell"
           :regex "^\\s*(let)?\\s*JJJ\\b\\s*(.+)?(?<==)"
           :tests ("test n = n * 2" "let test x y = x * y")
           :not ("nottest n = n * 2" "let testnot x y = x * y" "test $ y z"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "haskell"
           :regex "^\\s*(let)?\\s*JJJ\\b\\s*::"
           :tests ("test :: FilePath -> HttpSession [PkgIndexIndex]"
                   "test :: PackageId -> Tar.Entry -> PkgIndexInfo")
           :not ("nottest :: FilePath -> HttpSession [PkgIndexIndex]"
                 "testnot :: PackageId -> Tar.Entry -> PkgIndexInfo"))
    ;; lua
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test === 1234"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah)" "function(blah, test)")
           :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah)" "function(blah, testLast)"
                 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah)" "function(blah, Lasttest)"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
           :regex "function\\s*.+[.:]JJJ\\s*\\\("
           :tests ("function MyClass.test()" "function MyClass.test ()"
                   "function MyClass:test()" "function MyClass:test ()"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
           :regex "\\b.+\\.JJJ\\s*=\\s*function\\s*\\\("
           :tests ("MyClass.test = function()"))

    ;; rust
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\blet\\s+(\\\([^=\\n]*)?(mut\s+)?JJJ([^=\\n]*\\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+"
           :tests ("let test = 1234;"
                   "let test: u32 = 1234;"
                   "let test: Vec<u32> = Vec::new();"
                   "let mut test = 1234;"
                   "let mut test: Vec<u32> = Vec::new();"
                   "let (a, test, b) = (1, 2, 3);"
                   "let (a, mut test, mut b) = (1, 2, 3);"
                   "let (mut a, mut test): (u32, usize) = (1, 2);"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\bconst\\s+JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
           :tests ("const test: u32 = 1234;"))

    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\bstatic\\s+(mut\\s+)?JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
           :tests ("static test: u32 = 1234;"
                   "static mut test: u32 = 1234;"))

    ;; variable in method signature
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\bfn\\s+.+\\s*\\\((.+,\\s+)?JJJ:\\s*[^=\\n]+\\s*(,\\s*.+)*\\\)"
           :tests ("fn abc(test: u32) -> u32 {"
                   "fn abc(x: u32, y: u32, test: Vec<u32>, z: Vec<Foo>)"
                   "fn abc(x: u32, y: u32, test: &mut Vec<u32>, z: Vec<Foo>)"))

    ;; "if let" and "while let" desugaring
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
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
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "struct\\s+[^\\n{]+[{][^}]*(\\s*JJJ\\s*:\\s*[^\\n},]+)[^}]*}"
           :tests ("struct Foo { abc: u32, test: Vec<String>, b: PathBuf }"
                   "struct Foo<T>{test:Vec<T>}"
                   "struct FooBar<'a> { test: Vec<String> }")
           :not ("struct Foo { abc: u32, b: Vec<String> }"
                 "/// ... construct the equivalent ...\nfn abc() {\n"))

    ;; enum variants
    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "enum\\s+[^\\n{]+\\s*[{][^}]*\\bJJJ\\b[^}]*}"
           :tests ("enum Foo { VariantA, test, VariantB(u32) }"
                   "enum Foo<T> { test(T) }"
                   "enum BadStyle{test}"
                   "enum Foo32 { Bar, testing, test(u8) }")
           :not ("enum Foo { testing }"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\bfn\\s+JJJ\\s*\\\("
           :tests ("fn test(asdf: u32)" "fn test()" "pub fn test()"))

    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\bmacro_rules!\\s+JJJ"
           :tests ("macro_rules! test"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "struct\\s+JJJ\\s*[{\\\(]?"
           :tests ("struct test(u32, u32)"
                   "struct test;"
                   "struct test { abc: u32, def: Vec<String> }"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "trait\\s+JJJ\\s*[{]?"
           :tests ("trait test;" "trait test { fn abc() -> u32; }"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "\\btype\\s+JJJ([^=\\n]+)?\\s*=[^=\\n]+;"
           :tests ("type test<T> = Rc<RefCell<T>>;"
                   "type test = Arc<RwLock<Vec<u32>>>;"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "impl\\s+((\\w+::)*\\w+\\s+for\\s+)?(\\w+::)*JJJ\\s+[{]?"
           :tests ("impl test {"
                   "impl abc::test {"
                   "impl std::io::Read for test {"
                   "impl std::io::Read for abc::test {"))

    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
           :regex "mod\\s+JJJ\\s*[{]?"
           :tests ("mod test;" "pub mod test {")))

  "List of regex patttern templates organized by language and type to use for generating the grep command."
  :group 'dumb-jump
  :type
  '(repeat
    (plist
     :options ((:type string)
               (:supports string)
               (:language string)
               (:regex string)
               (:tests (repeat string))
               (:not (repeat string))))))

; https://github.com/ggreer/the_silver_searcher/blob/master/tests/list_file_types.t
(defcustom dumb-jump-language-file-exts
  '((:language "elisp" :ext "el" :agtype "elisp" :rgtype "elisp")
    (:language "elisp" :ext "el.gz" :agtype "elisp" :rgtype "elisp")
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
    (:language "haskell" :ext "hs" :agtype "haskell" :rgtype "haskell")
    (:language "haskell" :ext "lhs" :agtype "haskell" :rgtype "haskell")
    (:language "objc" :ext "m" :agtype "objc" :rgtype "objc")
    (:language "csharp" :ext "cs" :agtype "csharp" :rgtype "csharp")
    (:language "java" :ext "java" :agtype "java" :rgtype "java")
    (:language "clojure" :ext "clj" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljc" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljs" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljx" :agtype "clojure" :rgtype "clojure")
    (:language "coffeescript" :ext "coffee" :agtype "coffee" :rgtype "coffeescript")
    (:language "faust" :ext "dsp" :agtype nil :rgtype nil)
    (:language "faust" :ext "lib" :agtype nil :rgtype nil)
    (:language "fortran" :ext "f" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f77" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f90" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f95" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f03" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "for" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "ftn" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "fpp" :agtype "fortran" :rgtype "fortran")
    (:language "go" :ext "go" :agtype "go" :rgtype "go")
    (:language "javascript" :ext "js" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "jsx" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "vue" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "html" :agtype "html" :rgtype "html")
    (:language "lisp" :ext "lisp" :agtype "lisp" :rgtype "lisp")
    (:language "lisp" :ext "lsp" :agtype "lisp" :rgtype "lisp")
    (:language "lua" :ext "lua" :agtype "lua" :rgtype "lua")
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
    (:language "ruby" :ext "rb" :agtype "ruby" :rgtype "ruby")
    (:language "ruby" :ext "haml" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "slim" :agtype "ruby" :rgtype nil)
    (:language "rust" :ext "rs" :agtype "rust" :rgtype "rust")
    (:language "scala" :ext "scala" :agtype "scala" :rgtype "scala")
    (:language "scheme" :ext "scm" :agtype "scheme" :rgtype "lisp")
    (:language "shell" :ext "sh" :agtype "shell" :rgtype "sh")
    (:language "shell" :ext "bash" :agtype "shell" :rgtype "sh")
    (:language "shell" :ext "csh" :agtype "shell" :rgtype "sh")
    (:language "shell" :ext "ksh" :agtype "shell" :rgtype "sh")
    (:language "shell" :ext "tcsh" :agtype "shell" :rgtype "sh")
    (:language "swift" :ext "swift" :agtype "swift" :rgtype "swift"))
  "Mapping of programming language(s) to file extensions."
  :group 'dumb-jump
  :type
  '(repeat
    (plist
     :options ((:language (string :tag "Language"))
               (:ext (string :tag "Extension"))
               (:agtype (string :tag "Ag type"))
               (:rgtype (string :tag "Ripgrep type"))))))

(defcustom dumb-jump-language-contexts
  '((:language "javascript" :type "function" :right "^(" :left nil)
    (:language "javascript" :type "variable" :right nil :left "($")
    (:language "javascript" :type "variable" :right "^)" :left "($")
    (:language "javascript" :type "variable" :right "^\\." :left nil)
    (:language "javascript" :type "variable" :right "^;" :left nil)
    (:language "perl" :type "function" :right "^(" :left nil)
    (:language "elisp" :type "function" :right nil :left "($")
    (:language "elisp" :type "variable" :right "^)" :left nil))

  "List of under points contexts for each language.
This helps limit the number of regular expressions we use
if we know that if there's a '(' immediately to the right of
a symbol then it's probably a function call"
  :group 'dumb-jump
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

(defcustom dumb-jump-project-denoters
  '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "PkgInfo" "-pkg.el")
  "Files and directories that signify a directory is a project root."
  :group 'dumb-jump
  :type '(repeat (string  :tag "Name")))

(defcustom dumb-jump-default-project "~"
  "The default project to search within if a project root is not found."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-after-jump-hook nil
  "Hooks called after jumping."
  :type 'hook
  :group 'dumb-jump
  :type 'hook)

(defcustom dumb-jump-aggressive
  t
  "If `t' jump aggressively with the possiblity of a false
positive. If `nil' always show list of more than 1 match."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-debug
  nil
  "If `t` will print helpful debug information."
  :group 'dumb-jump
  :type 'boolean)


(defun dumb-jump-message-prin1 (str &rest args)
  "Helper function when debugging apply STR 'prin1-to-string' to all ARGS."
  (apply 'message str (-map 'prin1-to-string args)))

(defvar dumb-jump--ag-installed? 'unset)
(defun dumb-jump-ag-installed? ()
  "Return t if ag is installed."
  (if (eq dumb-jump--ag-installed? 'unset)
      (setq dumb-jump--ag-installed?
            (s-contains? "ag version" (shell-command-to-string (concat dumb-jump-ag-cmd " --version"))))
    dumb-jump--ag-installed?))

(defvar dumb-jump--rg-installed? 'unset)
(defun dumb-jump-rg-installed? ()
  "Return t if rg is installed."
  (if (eq dumb-jump--rg-installed? 'unset)
      (setq dumb-jump--rg-installed?
            (s-contains? "ripgrep" (shell-command-to-string (concat dumb-jump-rg-cmd " --version"))))
    dumb-jump--rg-installed?))

(defvar dumb-jump--git-grep-installed? 'unset)
(defun dumb-jump-git-grep-installed? ()
  "Return t if git-grep is installed."
  (if (eq dumb-jump--git-grep-installed? 'unset)
      (setq dumb-jump--git-grep-installed?
            (s-contains? "fatal: no pattern given"
                         (shell-command-to-string (concat dumb-jump-git-grep-cmd))))
    dumb-jump--git-grep-installed?))

(defvar dumb-jump--grep-installed? 'unset)
(defun dumb-jump-grep-installed? ()
  "Return 'gnu if GNU grep is installed, 'bsd if BSD grep is installed, and nil otherwise."
  (if (eq dumb-jump--grep-installed? 'unset)
      (let* ((version (shell-command-to-string (concat dumb-jump-grep-cmd " --version")))
             (variant (cond ((s-match "GNU grep" version) 'gnu)
                            ((s-match "[0-9]+\\.[0-9]+" version) 'bsd)
                            (t nil))))
        (setq dumb-jump--grep-installed? variant))
    dumb-jump--grep-installed?))

(defun dumb-jump-find-start-pos (line-in look-for cur-pos)
  "Find start column position for LINE-IN of LOOK-FOR using CUR-POS as a hint."
  (let ((is-found nil)
        (line (s-replace "\t" (s-repeat tab-width " ") line-in)))
    (while (and (> cur-pos 0) (not is-found))
      (let* ((char (substring line cur-pos (1+ cur-pos)))
             (is-at (s-index-of char look-for)))
        (if (null is-at)
            (setq is-found t)
          (setq cur-pos (1- cur-pos)))))
    (1+ cur-pos)))

(defun dumb-jump-run-test (test cmd)
  "Run CMD using string TEST as its standard input."
  (with-temp-buffer
    (insert test)
    (shell-command-on-region (point-min) (point-max) cmd nil t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dumb-jump-run-git-grep-test (test cmd)
  "Run CMD using string TEST as its input through a local,
temporary file. Because git grep must be given a file as input,
not just a string."
  (let* ((thefile ".git.grep.test")
         (realcmd (concat cmd " " thefile)))
    (with-temp-buffer
      (insert test)
      (write-file thefile nil)
      (delete-region (point-min) (point-max))
      (shell-command realcmd t)
      (delete-file thefile)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun dumb-jump-test-rules (&optional run-not-tests)
  "Test all the grep rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules."
  (let ((failures '())
        (fail-tmpl "grep %s FAILURE '%s' %s in response '%s' | CMD: '%s' | regex: '%s'")
        (variant (if (eq (dumb-jump-grep-installed?) 'gnu) 'gnu-grep 'grep)))
    (-each (--filter (member "grep" (plist-get it :supports)) dumb-jump-find-rules)
      (lambda (rule)
        (-each (plist-get rule (if run-not-tests :not :tests))
          (lambda (test)
            (let* ((cmd (concat "grep -En -e "
                                (shell-quote-argument (dumb-jump-populate-regex (plist-get rule :regex) "test" variant))))
                   (resp (dumb-jump-run-test test cmd)))
              (when (or
                     (and (not run-not-tests) (not (s-contains? test resp)))
                     (and run-not-tests (> (length resp) 0)))
                (add-to-list 'failures (format fail-tmpl (if run-not-tests "not" "")
                                               test (if run-not-tests "IS unexpectedly" "NOT") resp cmd (plist-get rule :regex)))))))))
    failures))

(defun dumb-jump-test-ag-rules (&optional run-not-tests)
  "Test all the ag rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules"
  (let ((failures '())
        (fail-tmpl "ag FAILURE '%s' %s in response '%s' | CMD: '%s' | rule: '%s'"))
    (-each (--filter (member "ag" (plist-get it :supports)) dumb-jump-find-rules)
      (lambda (rule)
        (-each (plist-get rule (if run-not-tests :not :tests))
          (lambda (test)
            (let* ((cmd (concat "ag --nocolor --nogroup "
                                (shell-quote-argument (dumb-jump-populate-regex (plist-get rule :regex) "test" 'ag))))
                   (resp (dumb-jump-run-test test cmd)))
              (when (or
                     (and (not run-not-tests) (not (s-contains? test resp)))
                     (and run-not-tests (> (length resp) 0)))
                (add-to-list 'failures (format fail-tmpl test (if run-not-tests "IS unexpectedly" "NOT") resp cmd rule))))))))
    failures))

(defun dumb-jump-test-rg-rules (&optional run-not-tests)
  "Test all the rg rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules"
  (let ((failures '())
        (fail-tmpl "rg FAILURE '%s' %s in response '%s' | CMD: '%s' | rule: '%s'"))
    (-each (--filter (member "rg" (plist-get it :supports)) dumb-jump-find-rules)
      (lambda (rule)
        (-each (plist-get rule (if run-not-tests :not :tests))
          (lambda (test)
            (let* ((cmd (concat "rg --color never --no-heading "
                                (shell-quote-argument (dumb-jump-populate-regex (plist-get rule :regex) "test" 'rg))))
                   (resp (dumb-jump-run-test test cmd)))
              (when (or
                     (and (not run-not-tests) (not (s-contains? test resp)))
                     (and run-not-tests (> (length resp) 0)))
                (add-to-list 'failures (format fail-tmpl test (if run-not-tests "IS unexpectedly" "NOT") resp cmd rule))))))))
    failures))

(defun dumb-jump-test-git-grep-rules (&optional run-not-tests)
  "Test all the git grep rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules"
  (let ((failures '())
        (fail-tmpl "git grep FAILURE '%s' %s in response '%s' | CMD: '%s' | rule: '%s'"))
    (-each (--filter (member "git-grep" (plist-get it :supports)) dumb-jump-find-rules)
      (lambda (rule)
        (-each (plist-get rule (if run-not-tests :not :tests))
          (lambda (test)
            (let* ((cmd (concat "git grep --color=never -h --untracked -E "
                                (shell-quote-argument
                                 (dumb-jump-populate-regex (plist-get rule :regex) "test" 'git-grep))))
                   (resp (dumb-jump-run-git-grep-test test cmd)))
              (when (or
                     (and (not run-not-tests) (not (s-contains? test resp)))
                     (and run-not-tests (> (length resp) 0)))
                (add-to-list 'failures (format fail-tmpl test (if run-not-tests "IS unexpectedly" "NOT") resp cmd rule))))))))
    failures))

(defun dumb-jump-message (str &rest args)
  "Log message STR with ARGS to the *Messages* buffer if not using dumb-jump-quiet."
  (when (not dumb-jump-quiet)
    (apply 'message str args)))

(defun dumb-jump-get-point-context (line func cur-pos)
  "Get the LINE context to the left and right of FUNC using CUR-POS as hint."
  (let* ((loc (dumb-jump-find-start-pos line func cur-pos))
         (func-len (length func))
         (sen-len (length line))
         (right-loc-start (+ loc func-len))
         (right-loc-end (length line))
         (left (substring line 0 loc))
         (right (if (> right-loc-end sen-len)
                    ""
                  (substring line right-loc-start right-loc-end))))
    `(:left ,left :right ,right)))

(defun dumb-jump-to-selected (results choices selected)
  "With RESULTS use CHOICES to find the SELECTED choice from multiple options."
  (let* ((result-index (--find-index (string= selected it) choices))
         (result (when result-index
                   (nth result-index results))))
        (when result
          (dumb-jump-result-follow result))))

(defun dumb-jump-helm-persist-action (match)
  "Previews a MATCH in a temporary buffer at the matched line
number when pressing C-j in helm."
  (let* ((parts (--remove (string= it "")
                          (s-split "\\(?:^\\|:\\)[0-9]+:"  match)))
         (file-line-part (s-split ":" (nth 0 parts)))
         (file (nth 0 file-line-part))
         (line (string-to-number (nth 1 file-line-part)))
         (default-directory-old default-directory))
    (switch-to-buffer (get-buffer-create " *helm dumb jump persistent*"))
    (setq default-directory default-directory-old)
    (fundamental-mode)
    (erase-buffer)
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (set-auto-mode)
      (font-lock-fontify-region (point-min) (point-max))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun dumb-jump-prompt-user-for-choice (proj results)
  "Put a PROJ's list of RESULTS in a 'popup-menu' (or helm/ivy)
for user to select.  Filters PROJ path from files for display."
  (let* ((choices (-map (lambda (result)
                          (format "%s:%s %s"
                                  (s-replace proj "" (plist-get result :path))
                                  (plist-get result :line)
                                  (s-trim (plist-get result :context))))
                        results)))
    (cond
     ((and (eq dumb-jump-selector 'ivy) (fboundp 'ivy-read))
      (dumb-jump-to-selected results choices (ivy-read "Jump to: " choices)))
     ((and (eq dumb-jump-selector 'helm) (fboundp 'helm))
      (dumb-jump-to-selected results choices
                             (helm :sources
                                   (helm-build-sync-source "Jump to: "
                                     :candidates choices
                                     :persistent-action 'dumb-jump-helm-persist-action)
                                   :buffer "*helm dumb jump choices*")))
     (t
      (dumb-jump-to-selected results choices (popup-menu* choices))))))

(defun dumb-jump-get-project-root (filepath)
  "Keep looking at the parent dir of FILEPATH until a denoter file/dir is found."
  (f-expand
    (or
      (locate-dominating-file filepath #'dumb-jump-get-config)
      dumb-jump-default-project)))

(defun dumb-jump-get-config (dir)
  "If a project denoter is in DIR then return it, otherwise
nil. However, if DIR contains a `.dumbjumpignore' it returns nil
to keep looking for another root."
  (if (f-exists? (f-join dir ".dumbjumpignore"))
      nil
    (car (--filter
          (f-exists? (f-join dir it))
          dumb-jump-project-denoters))))

(defun dumb-jump-get-language (file)
  "Get language from FILE extension and then fallback to using 'major-mode' name."
  (let* ((languages (-distinct
                     (--map (plist-get it :language)
                            dumb-jump-find-rules)))
         (language (or (dumb-jump-get-language-by-filename file)
                       (dumb-jump-get-language-from-mode))))
    (if (member language languages)
      language
      (format ".%s file" (or (f-ext file) "?")))))

(defun dumb-jump-get-language-from-mode ()
  "Extract the language from the 'major-mode' name.  Currently just everything before '-mode'."
  (s-replace "-mode" "" (symbol-name major-mode)))

(defun dumb-jump-get-language-by-filename (file)
  "Get the programming language from the FILE."
  (let* ((filename (if (s-ends-with? ".gz" file)
                       (f-no-ext file)
                     file))
         (result (--filter
                  (s-ends-with? (concat "." (plist-get it :ext)) filename)
                  dumb-jump-language-file-exts)))
    (when result
        (plist-get (car result) :language))))

(defun dumb-jump-issue-result (issue)
  "Return a result property list with the ISSUE set as :issue property symbol."
  `(:results nil :lang nil :symbol nil :ctx-type nil :file nil :root nil :issue ,(intern issue)))

(defun dumb-jump-get-results (&optional prompt)
  "Run dumb-jump-fetch-results if searcher installed, buffer is saved, and there's a symbol under point."
  (cond
   ((not (or (dumb-jump-ag-installed?)
             (dumb-jump-rg-installed?)
             (dumb-jump-git-grep-installed?)
             (dumb-jump-grep-installed?)))
    (dumb-jump-issue-result "nogrep"))
   ((or (string= (buffer-name) "*shell*")
        (string= (buffer-name) "*eshell*"))
    (dumb-jump-fetch-shell-results prompt))
   ((buffer-modified-p (current-buffer))
    (dumb-jump-issue-result "unsaved"))
   ((and (not prompt) (not (region-active-p)) (not (thing-at-point 'symbol)))
    (dumb-jump-issue-result "nosymbol"))
   (t
    (dumb-jump-fetch-file-results prompt))))

(defun dumb-jump-fetch-shell-results (&optional prompt)
  (let* ((cur-file (buffer-name))
         (proj-root (dumb-jump-get-project-root default-directory))
         (proj-config (dumb-jump-get-config proj-root))
         (config (when (s-ends-with? ".dumbjump" proj-config)
                   (dumb-jump-read-config proj-root proj-config)))
         (lang (or (plist-get config :language)
                   (car (dumb-jump-get-lang-by-shell-contents (buffer-name))))))
    (dumb-jump-fetch-results cur-file proj-root lang config prompt)))

(defun dumb-jump-fetch-file-results (&optional prompt)
  (let* ((cur-file (or (buffer-file-name) ""))
         (proj-root (dumb-jump-get-project-root cur-file))
         (proj-config (dumb-jump-get-config proj-root))
         (config (when (s-ends-with? ".dumbjump" proj-config)
                   (dumb-jump-read-config proj-root proj-config)))
         (lang (or (plist-get config :language)
                   (dumb-jump-get-language cur-file))))
    (dumb-jump-fetch-results cur-file proj-root lang config prompt)))

(defun dumb-jump-process-symbol-by-lang (lang look-for)
  "Process LANG's LOOK-FOR.  For instance, clojure needs namespace part removed."
  (cond
   ((and (string= lang "clojure") (s-contains? "/" look-for))
    (nth 1 (s-split "/" look-for)))
   ((and (string= lang "ruby") (s-starts-with? ":" look-for))
    (s-chop-prefix ":" look-for))
   (t
    look-for)))

(defun dumb-jump-get-point-line ()
  "Get line at point."
  (if (version< emacs-version "24.4")
      (thing-at-point 'line)
    (thing-at-point 'line t)))

(defun dumb-jump-get-point-symbol ()
  "Get symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (version< emacs-version "24.4")
        (thing-at-point 'symbol)
      (thing-at-point 'symbol t))))

(defun dumb-jump-get-lang-by-shell-contents (buffer)
  "Return languages in BUFFER by checking if file extension is mentioned."
  (let* ((buffer-contents (with-current-buffer buffer
                           (buffer-string)))

        (found (--filter (s-match (concat "\\." (plist-get it :ext) "\\b") buffer-contents)
              dumb-jump-language-file-exts)))
    (--map (plist-get it :language) found)))

(defun dumb-jump-fetch-results (cur-file proj-root lang config &optional prompt)
  "Return a list of results based on current file context and calling grep/ag.
CUR-FILE is the path of the current buffer.
PROJ-ROOT is that file's root project directory.
LANG is a string programming langage with CONFIG a property list
of project configuraiton."
  (let* ((cur-line (if prompt 0 (dumb-jump-get-point-line)))
         (look-for-start (when (not prompt)
                           (- (car (bounds-of-thing-at-point 'symbol))
                            (point-at-bol))))
         (cur-line-num (line-number-at-pos))
         (proj-config (dumb-jump-get-config proj-root))
         (config (when (s-ends-with? ".dumbjump" proj-config)
                   (dumb-jump-read-config proj-root proj-config)))
         (found-symbol (or prompt (dumb-jump-get-point-symbol)))
         (look-for (or prompt (dumb-jump-process-symbol-by-lang lang found-symbol)))
         (pt-ctx (if (and (not prompt) (not (string= cur-line look-for)))
                     (dumb-jump-get-point-context cur-line look-for look-for-start)
                   nil))
         (ctx-type
          (dumb-jump-get-ctx-type-by-language lang pt-ctx))

         (gen-funcs (dumb-jump-pick-grep-variant proj-root))
         (parse-fn (plist-get gen-funcs :parse))
         (generate-fn (plist-get gen-funcs :generate))
         (searcher (plist-get gen-funcs :searcher))

         (regexes (dumb-jump-get-contextual-regexes lang ctx-type searcher))

         (exclude-paths (when config (plist-get config :exclude)))
         (include-paths (when config (plist-get config :include)))
         ; we will search proj root and all include paths
         (search-paths (-distinct (-concat (list proj-root) include-paths)))
         ; run command for all
         (raw-results (--mapcat
                       ;; TODO: should only pass exclude paths to actual project root
                       (dumb-jump-run-command look-for it regexes lang exclude-paths cur-file
                                              cur-line-num parse-fn generate-fn)
                       search-paths))

         (results (--map (plist-put it :target look-for) raw-results)))

    `(:results ,results :lang ,(if (null lang) "" lang) :symbol ,look-for :ctx-type ,(if (null ctx-type) "" ctx-type) :file ,cur-file :root ,proj-root)))

;;;###autoload
(defun dumb-jump-back ()
  "Jump back to where the last jump was done."
  (interactive)
  (pop-tag-mark)
  (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
    (run-hooks 'dumb-jump-after-jump-hook)))

;;;###autoload
(defun dumb-jump-quick-look ()
  "Run dumb-jump-go in quick look mode.  That is, show a tooltip of where it would jump instead."
  (interactive)
  (dumb-jump-go t))

;;;###autoload
(defun dumb-jump-go-other-window ()
  "Like 'dumb-jump-go' but use 'find-file-other-window' instead of 'find-file'."
  (interactive)
  (let ((dumb-jump-window 'other))
        (dumb-jump-go)))

;;;###autoload
(defun dumb-jump-go-current-window ()
  "Like dumb-jump-go but always use 'find-file'."
  (interactive)
  (let ((dumb-jump-window 'current))
        (dumb-jump-go)))

;;;###autoload
(defun dumb-jump-go-prefer-external ()
  "Like dumb-jump-go but prefer external matches from the current file."
  (interactive)
  (dumb-jump-go nil t))

;;;###autoload
(defun dumb-jump-go-prompt ()
  "Like dumb-jump-go but prompts for function instead of using under point"
  (interactive)
  (dumb-jump-go nil nil (read-from-minibuffer "Jump to: ")))

;;;###autoload
(defun dumb-jump-go-prefer-external-other-window ()
  "Like dumb-jump-go-prefer-external but use 'find-file-other-window' instead of 'find-file'."
  (interactive)
  (let ((dumb-jump-window 'other))
    (dumb-jump-go-prefer-external)))

;;;###autoload
(defun dumb-jump-go (&optional use-tooltip prefer-external prompt)
  "Go to the function/variable declaration for thing at point.
When USE-TOOLTIP is t a tooltip jump preview will show instead.
When PREFER-EXTERNAL is t it will sort external matches before
current file."
  (interactive "P")
  (let* ((start-time (float-time))
         (info (dumb-jump-get-results prompt))
         (end-time (float-time))
         (fetch-time (- end-time start-time))
         (results (plist-get info :results))
         (look-for (or prompt (plist-get info :symbol)))
         (proj-root (plist-get info :root))
         (issue (plist-get info :issue))
         (lang (plist-get info :lang))
         (result-count (length results)))
    (cond
     ((> fetch-time dumb-jump-max-find-time)
      (dumb-jump-message
       "Took over %ss to find '%s'. Please install ag or rg, or add a .dumbjump file to '%s' with path exclusions"
       (number-to-string dumb-jump-max-find-time) look-for proj-root))
     ((eq issue 'unsaved)
      (dumb-jump-message "Please save your file before jumping."))
     ((eq issue 'nogrep)
      (dumb-jump-message "Please install ag, rg, git grep or grep!"))
     ((eq issue 'nosymbol)
      (dumb-jump-message "No symbol under point."))
     ((s-ends-with? " file" lang)
      (dumb-jump-message "Could not find rules for '%s'." lang))
     ((= result-count 1)
      (dumb-jump-result-follow (car results) use-tooltip proj-root))
     ((> result-count 1)
      ;; multiple results so let the user pick from a list
      ;; unless the match is in the current file
      (dumb-jump-handle-results results (plist-get info :file) proj-root (plist-get info :ctx-type)
                                look-for use-tooltip prefer-external))
     ((= result-count 0)
      (dumb-jump-message "'%s' %s %s declaration not found." look-for (if (s-blank? lang) "with unknown language so" lang) (plist-get info :ctx-type))))))

(defcustom dumb-jump-language-comments
  '((:comment "//" :language "c++")
    (:comment ";" :language "elisp")
    (:comment "//" :language "javascript")
    (:comment "--" :language "haskell")
    (:comment "--" :language "lua")
    (:comment "//" :language "rust")
    (:comment "//" :language "objc")
    (:comment "//" :language "csharp")
    (:comment "//" :language "java")
    (:comment ";" :language "clojure")
    (:comment "#" :language "coffeescript")
    (:comment "//" :language "faust")
    (:comment "!" :language "fortran")
    (:comment "//" :language "go")
    (:comment ";" :language "lisp")
    (:comment "#" :language "perl")
    (:comment "//" :language "php")
    (:comment "#" :language "python")
    (:comment "#" :language "r")
    (:comment "#" :language "ruby")
    (:comment "//" :language "scala")
    (:comment ";" :language "scheme")
    (:comment "#" :language "shell")
    (:comment "//" :language "swift"))
  "List of one-line comments organized by language."
  :group 'dumb-jump
  :type
  '(repeat
    (plist
     :options ((:comment string)
               (:language string)))))

(defun dumb-jump-get-comment-by-language (lang)
  "Yields the one-line comment for the given LANG."
  (let* ((entries (-distinct
                   (--filter (string= (plist-get it :language) lang)
                             dumb-jump-language-comments))))
    (if (= 1 (length entries))
        (plist-get (car entries) :comment)
      nil)))

(defun dumb-jump-filter-no-start-comments (results lang)
  "Filter out RESULTS with a :context that starts with a comment
given the LANG of the current file."
  (let ((comment (dumb-jump-get-comment-by-language lang)))
    (if comment
        (-concat
         (--filter (not (s-starts-with? comment (s-trim (plist-get it :context)))) results))
      results)))

(defun dumb-jump-handle-results
    (results cur-file proj-root ctx-type look-for use-tooltip prefer-external)
  "Handle the searchers results.
RESULTS is a list of property lists with the searcher's results.
CUR-FILE is the current file within PROJ-ROOT.
CTX-TYPE is a string of the current context.
LOOK-FOR is the symbol we're jumping for.
USE-TOOLTIP shows a preview instead of jumping.
PREFER-EXTERNAL will sort current file last."
  "Figure which of the RESULTS to jump to. Favoring the CUR-FILE"
  (let* ((lang (dumb-jump-get-language-by-filename cur-file))
         (match-sorted (-sort (lambda (x y) (< (plist-get x :diff) (plist-get y :diff))) results))
         (match-no-comments (dumb-jump-filter-no-start-comments match-sorted lang))

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
               (append (dumb-jump-current-file-results cur-file match-cur-file-front)
                       (dumb-jump-current-file-results rel-cur-file match-cur-file-front)))
            match-cur-file-front))

         (var-to-jump (car matches))
         ;; TODO: handle if ctx-type is null but ALL results are variable

         ;; When non-aggressive it should only jump when there is only one match, regardless of
         ;; context.
         (do-var-jump
          (and (or dumb-jump-aggressive
                   (= (length match-cur-file-front) 1))
               (or (= (length matches) 1)
                   (string= ctx-type "variable")
                   (string= ctx-type ""))
               var-to-jump)))

    (when dumb-jump-debug
      (dumb-jump-message
       "-----\nDUMB JUMP DEBUG `dumb-jump-handle-results` START\n----- \n\nlook for: \n\t%s\n\ntype: \n\t%s \n\njump? \n\t%s \n\nmatches: \n\t%s \n\nresults: \n\t%s \n\nprefer external: \n\t%s\n\nmatch-cur-file-front: \n\t%s\n\nproj-root: \n\t%s\n\ncur-file: \n\t%s\n\nreal-cur-file: \n\t%s \n\n-----\nDUMB JUMP DEBUG `dumb-jump-handle-results` END\n-----\n"
       look-for ctx-type var-to-jump (pp-to-string match-cur-file-front) (pp-to-string results) prefer-external match-cur-file-front proj-root cur-file rel-cur-file))
    (if do-var-jump
        (dumb-jump-result-follow var-to-jump use-tooltip proj-root)
      (dumb-jump-prompt-user-for-choice proj-root match-cur-file-front))))

(defun dumb-jump-read-config (root config-file)
  "Load and return options (exclusions, inclusions, etc).
Ffrom the ROOT project CONFIG-FILE."
  (let* ((contents (f-read-text (f-join root config-file)))
         (lines (s-split "\n" contents))
         (lang-match (s-match "^language \\\(.+\\\)$" contents))
         (lang (when (= (length lang-match) 2) (nth 1 lang-match)))
         (exclude-lines (--filter (s-starts-with? "-" it) lines))
         (include-lines (--filter (s-starts-with? "+" it) lines))
         (exclude-paths (-map (lambda (f)
                                 (let* ((dir (substring f 1))
                                       (use-dir (if (s-starts-with? "/" dir)
                                                    (substring dir 1)
                                                    dir)))
                                   (f-join root use-dir)))
                               exclude-lines))
         (include-paths (-map (lambda (f)
                                (let* ((dir (substring f 1)))
                                  (if (s-starts-with? "/" dir)
                                      dir ;; absolute paths are allowed
                                    ;; TODO: warn if an include path is already a child of proj-root
                                    (f-join root dir))))
                              include-lines)))

    `(:exclude ,exclude-paths :include ,include-paths :language ,lang)))

(defun dumb-jump-result-follow (result &optional use-tooltip proj)
  "Take the RESULT to jump to and record the jump, for jumping back, and then trigger jump."
  (let* ((target-boundary (s-matched-positions-all
                           (concat "\\b" (regexp-quote (plist-get result :target)) "\\b")
                           (plist-get result :context)))
         ;; column pos is either via tpos from ag or by using the regex above or last using old s-index-of
         (pos (if target-boundary
                  (car (car target-boundary))
                (s-index-of (plist-get result :target) (plist-get result :context))))

        (thef (plist-get result :path))
        (line (plist-get result :line)))
    (when thef
      (if use-tooltip
          (popup-tip (format "%s:%s %s"
                                  (s-replace proj "" (plist-get result :path))
                                  (plist-get result :line)
                                  (s-trim (plist-get result :context))))
          (dumb-jump-goto-file-line thef line pos)))
    ; return the file for test
    thef))


(defun dumb-jump-goto-file-line (thefile theline pos)
  "Open THEFILE and go line THELINE"
  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
   (ring-insert find-tag-marker-ring (point-marker)))

  (let* ((visible-buffer (find-buffer-visiting thefile))
         (visible-window (when visible-buffer (get-buffer-window visible-buffer))))
    (cond
     ((and visible-window dumb-jump-use-visible-window)
      (select-window visible-window))
     ((eq dumb-jump-window 'other)
      (find-file-other-window thefile))
     (t (find-file thefile))))

  (goto-char (point-min))
  (forward-line (1- theline))
  (forward-char pos)
  (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
    (run-hooks 'dumb-jump-after-jump-hook)))

(defun dumb-jump-current-file-results (path results)
  "Return the PATH's RESULTS."
  (let ((matched (--filter (string= path (plist-get it :path)) results)))
    matched))

(defun dumb-jump-generators-by-searcher (searcher)
  "For a SEARCHER it yields a response parser, a command
generator function, an installed? function, and the corresponding
searcher symbol."
  (cond ((equal 'git-grep searcher)
         `(:parse ,'dumb-jump-parse-git-grep-response
                  :generate ,'dumb-jump-generate-git-grep-command
                  :installed ,'dumb-jump-git-grep-installed?
                  :searcher ,searcher))
        ((equal 'ag searcher)
         `(:parse ,'dumb-jump-parse-ag-response
                  :generate ,'dumb-jump-generate-ag-command
                  :installed ,'dumb-jump-ag-installed?
                  :searcher ,searcher))
        ((equal 'rg searcher)
         `(:parse ,'dumb-jump-parse-rg-response
                  :generate ,'dumb-jump-generate-rg-command
                  :installed ,'dumb-jump-rg-installed?
                  :searcher ,searcher))
        ((equal 'gnu-grep searcher)
         `(:parse ,'dumb-jump-parse-grep-response
                  :generate ,'dumb-jump-generate-gnu-grep-command
                  :installed ,'dumb-jump-grep-installed?
                  :searcher ,searcher))
        ((equal 'grep searcher)
         `(:parse ,'dumb-jump-parse-grep-response
                  :generate ,'dumb-jump-generate-grep-command
                  :installed ,'dumb-jump-grep-installed?
                  :searcher ,searcher))))

(defun dumb-jump-pick-grep-variant (&optional proj-root)
  (cond
   ;; If `dumb-jump-force-searcher' is not nil then use that searcher.
   (dumb-jump-force-searcher
    (dumb-jump-generators-by-searcher dumb-jump-force-searcher))

   ;; If project root has a .git then use git-grep if installed.
   ((and proj-root
         (dumb-jump-git-grep-installed?)
         (f-exists? (f-join proj-root ".git")))
    (dumb-jump-generators-by-searcher 'git-grep))

   ;; If `dumb-jump-prefer-searcher' is not nil then use if installed.
   ((and dumb-jump-prefer-searcher
         (funcall (plist-get (dumb-jump-generators-by-searcher dumb-jump-prefer-searcher)
                             :installed)))
    (dumb-jump-generators-by-searcher dumb-jump-prefer-searcher))

   ;; Fallback searcher order.
   ((dumb-jump-ag-installed?)
    (dumb-jump-generators-by-searcher 'ag))
   ((dumb-jump-rg-installed?)
    (dumb-jump-generators-by-searcher 'rg))
   ((eq (dumb-jump-grep-installed?) 'gnu)
    (dumb-jump-generators-by-searcher 'gnu-grep))
   (t
    (dumb-jump-generators-by-searcher 'grep))))

(defun dumb-jump-shell-command-switch ()
  "Yields the shell command switch to use for the current
  `shell-file-name' in order to not load the shell profile/RC for
  speeding up things."
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

;; TODO: rename dumb-jump-run-definition-command
(defun dumb-jump-run-command
    (look-for proj regexes lang exclude-args cur-file line-num parse-fn generate-fn)
  "Run the grep command based on the needle LOOK-FOR in the directory TOSEARCH"
  (let* ((cmd (funcall generate-fn look-for cur-file proj regexes lang exclude-args))
         (shell-command-switch (dumb-jump-shell-command-switch))
         (rawresults (shell-command-to-string cmd)))

    (when dumb-jump-debug
      (dumb-jump-message
       "-----\nDUMB JUMP DEBUG `dumb-jump-run-command` START\n----- \n\ncmd: \n\t%s\n\nraw results: \n\n\t%s \n\n-----\nDUMB JUMP DEBUG `dumb-jump-run-command` END\n-----\n" cmd rawresults))

    (when (and (s-blank? rawresults) dumb-jump-fallback-search)
      (setq regexes (list dumb-jump-fallback-regex))
      (setq cmd (funcall generate-fn look-for cur-file proj regexes lang exclude-args))
      (setq rawresults (shell-command-to-string cmd))
      (when dumb-jump-debug
        (dumb-jump-message
       "-----\nDUMB JUMP DEBUG `dumb-jump-run-command` (FALLBACK!) START\n----- \n\ncmd: \n\t%s\n\nraw results: \n\t%s \n\n-----\nDUMB JUMP DEBUG `dumb-jump-run-command` (FALLBACK) END\n-----\n" cmd rawresults)))

    (unless (s-blank? cmd)
      (let ((results (funcall parse-fn rawresults cur-file line-num)))
        (--filter (s-contains? look-for (plist-get it :context)) results)))))

(defun dumb-jump-parse-response-line (resp-line cur-file)
  "Parse a search program's single RESP-LINE for CUR-FILE into a list of (path line context)."
  (let* ((parts (--remove (string= it "")
                          (s-split "\\(?:^\\|:\\)[0-9]+:"  resp-line)))
         (line-num-raw (s-match "\\(?:^\\|:\\)\\([0-9]+\\):" resp-line)))

    (cond
     ;; fixes rare bug where context is blank  but file is defined "/somepath/file.txt:14:"
     ;; OR: (and (= (length parts) 1) (f-exists? (f-join (nth 0 parts))))
     ((s-match ":[0-9]+:$" resp-line)
      nil)
     ((and parts line-num-raw)
      (if (= (length parts) 2)
          (list (f-join (nth 0 parts)) (nth 1 line-num-raw) (nth 1 parts))
                                        ; this case is when they are searching a particular file...
        (list (f-join cur-file) (nth 1 line-num-raw) (nth 0 parts)))))))

(defun dumb-jump-parse-response-lines (parsed cur-file cur-line-num)
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

(defun dumb-jump-parse-grep-response (resp cur-file cur-line-num)
  "Takes a grep response RESP and parses into a list of plists."
  (let* ((resp-no-warnings (--filter (and (not (s-starts-with? "grep:" it))
                                          (not (s-contains? "No such file or" it)))
                                     (s-split "\n" (s-trim resp))))
         (parsed (--map (dumb-jump-parse-response-line it cur-file) resp-no-warnings)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-parse-ag-response (resp cur-file cur-line-num)
  "Takes a ag response RESP and parses into a list of plists."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumb-jump-parse-response-line it cur-file) resp-lines)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-parse-rg-response (resp cur-file cur-line-num)
  "Takes a rg response RESP and parses into a list of plists."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumb-jump-parse-response-line it cur-file) resp-lines)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-parse-git-grep-response (resp cur-file cur-line-num)
  "Takes a git grep response RESP and parses into a list of plists."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumb-jump-parse-response-line it cur-file) resp-lines)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-re-match (re s)
  "Does regular expression RE match string S. If RE is nil return nil."
  (when (and re s)
    (s-match re s)))

(defun dumb-jump-get-ctx-type-by-language (lang pt-ctx)
  "Detect the type of context by the language LANG and its context PT-CTX."
  (let* ((contexts (--filter (string= (plist-get it ':language) lang) dumb-jump-language-contexts))
         (usable-ctxs
          (when (> (length contexts) 0)
              (--filter (and (or (null (plist-get it :left))
                                 (dumb-jump-re-match (plist-get it :left)
                                                     (plist-get pt-ctx :left)))
                             (or (null (plist-get it :right))
                                 (dumb-jump-re-match (plist-get it :right)
                                                     (plist-get pt-ctx :right))))
                        contexts)))
         (use-ctx (= (length (--filter
                              (string= (plist-get it ':type)
                                       (and usable-ctxs (plist-get (car usable-ctxs) :type)))
                              usable-ctxs))
                     (length usable-ctxs))))

    (when (and usable-ctxs use-ctx)
      (plist-get (car usable-ctxs) :type))))

(defun dumb-jump-get-ext-includes (language)
  "Generate the --include grep argument of file extensions by LANGUAGE."
  (let ((exts (dumb-jump-get-file-exts-by-language language)))
    (dumb-jump-arg-joiner
     "--include"
     (--map (format "\\*.%s" it) exts))))

(defun dumb-jump-arg-joiner (prefix values)
  "Helper to generate command arg with its PREFIX for each value in VALUES."
  (let ((args (s-join (format " %s " prefix) values)))
    (if (and args values)
      (format " %s %s " prefix args)
      "")))

(defun dumb-jump-get-contextual-regexes (lang ctx-type searcher)
  "Get list of search regular expressions by LANG and CTX-TYPE (variable, function, etc)."
  (let* ((raw-rules (dumb-jump-get-rules-by-language lang searcher))
         (ctx-type (unless dumb-jump-ignore-context ctx-type))
         (ctx-rules
          (if ctx-type
              (--filter (string= (plist-get it :type) ctx-type) raw-rules)
            raw-rules))
         (rules (or ctx-rules raw-rules))
         (regexes (--map (plist-get it :regex) rules)))
    regexes))

(defun dumb-jump-populate-regex (it look-for variant)
  "Populate IT regex template with LOOK-FOR."
  (let ((boundary (cond ((eq variant 'rg) dumb-jump-rg-word-boundary)
                        ((eq variant 'ag) dumb-jump-ag-word-boundary)
                        ((eq variant 'git-grep) dumb-jump-git-grep-word-boundary)
                        (t dumb-jump-grep-word-boundary))))
    (let ((text it))
      (setq text (s-replace "\\j" boundary text))
      (when (eq variant 'gnu-grep)
        (setq text (s-replace "\\s" "[[:space:]]" text)))
      (setq text (s-replace "JJJ" (regexp-quote look-for) text))
      (when (and (eq variant 'rg) (string-prefix-p "-" text))
        (setq text (concat "[-]" (substring text 1))))
      text)))

(defun dumb-jump-populate-regexes (look-for regexes variant)
  "Take list of REGEXES and populate the LOOK-FOR target and return that list."
  (--map (dumb-jump-populate-regex it look-for variant) regexes))

(defun dumb-jump-generate-ag-command (look-for cur-file proj regexes lang exclude-paths)
  "Generate the ag response based on the needle LOOK-FOR in the directory PROJ."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'ag))
         (agtypes (dumb-jump-get-ag-type-by-language lang))
         ;; TODO: --search-zip always? in case the include is the in gz area like emacs lisp code.
         (cmd (concat dumb-jump-ag-cmd
                      " --nocolor --nogroup"
                      (if (s-ends-with? ".gz" cur-file)
                          " --search-zip"
                        "")
                      (s-join "" (--map (format " --%s" it) agtypes))))
         (exclude-args (dumb-jump-arg-joiner
                        "--ignore-dir" (--map (shell-quote-argument (s-replace proj "" it)) exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd exclude-args regex-args proj))))

(defun dumb-jump-generate-rg-command (look-for cur-file proj regexes lang exclude-paths)
  "Generate the rg response based on the needle LOOK-FOR in the directory PROJ."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'rg))
         (rgtypes (dumb-jump-get-rg-type-by-language lang))
         (cmd (concat dumb-jump-rg-cmd
                      " --color never --no-heading --line-number"
                      (s-join "" (--map (format " --type %s" it) rgtypes))))
         (exclude-args (dumb-jump-arg-joiner
                        "-g" (--map (shell-quote-argument (concat "!" (s-replace proj "" it))) exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd exclude-args regex-args proj))))

(defun dumb-jump-generate-git-grep-command (look-for cur-file proj regexes lang exclude-paths)
  "Generate the git grep response based on the needle LOOK-FOR in the directory PROJ."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'git-grep))
         (ggtypes (dumb-jump-get-git-grep-type-by-language lang))
         (cmd (concat dumb-jump-git-grep-cmd
                      " --color=never --line-number"
                      (if dumb-jump-git-grep-search-untracked
                          " --untracked"
                        "")
                      " -E"))
         (fileexps (s-join " " (--map (shell-quote-argument (format "%s/*.%s" proj it)) ggtypes)))
         (exclude-args (s-join " "
                               (--map (shell-quote-argument (concat ":(exclude)" it))
                                      exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd regex-args "--" fileexps exclude-args))))

(defun dumb-jump-generate-grep-command (look-for cur-file proj regexes lang exclude-paths)
  "Find LOOK-FOR's CUR-FILE in the PROJ with REGEXES for the LANG but not in EXCLUDE-PATHS."
  (let* ((filled-regexes (--map (shell-quote-argument it)
                                (dumb-jump-populate-regexes look-for regexes 'grep)))
         (cmd (concat (if (eq system-type 'windows-nt) "" (concat dumb-jump-grep-prefix " "))
                      (if (s-ends-with? ".gz" cur-file)
                          dumb-jump-zgrep-cmd
                        dumb-jump-grep-cmd)))
         (exclude-args (dumb-jump-arg-joiner "--exclude-dir" exclude-paths))
         (include-args (dumb-jump-get-ext-includes lang))
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd dumb-jump-grep-args exclude-args include-args regex-args proj))))

(defun dumb-jump-generate-gnu-grep-command (look-for cur-file proj regexes lang exclude-paths)
  "Find LOOK-FOR's CUR-FILE in the PROJ with REGEXES for the LANG but not in EXCLUDE-PATHS."
  (let* ((filled-regexes (--map (shell-quote-argument it)
                                (dumb-jump-populate-regexes look-for regexes 'gnu-grep)))
         (cmd (concat (if (eq system-type 'windows-nt) "" (concat dumb-jump-grep-prefix " "))
                      (if (s-ends-with? ".gz" cur-file)
                          dumb-jump-zgrep-cmd
                        dumb-jump-grep-cmd)))
         ;; TODO: GNU grep doesn't support these, so skip them
         (exclude-args "")
         (include-args "")
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd dumb-jump-gnu-grep-args exclude-args include-args regex-args proj))))

(defun dumb-jump-concat-command (&rest parts)
  "Concat the PARTS of a command if each part has a length."
  (s-join " " (-map #'s-trim (--filter (> (length it) 0) parts))))

(defun dumb-jump-get-file-exts-by-language (language)
  "Return list of file extensions for a LANGUAGE."
  (--map (plist-get it :ext)
         (--filter (string= (plist-get it :language) language)
                  dumb-jump-language-file-exts)))

(defun dumb-jump-get-ag-type-by-language (language)
  "Return list of ag type argument for a LANGUAGE."
  (-distinct (--map (plist-get it :agtype)
                    (--filter (and
                               (plist-get it :agtype)
                               (string= (plist-get it :language) language))
                              dumb-jump-language-file-exts))))

(defun dumb-jump-get-rg-type-by-language (language)
  "Return list of rg type argument for a LANGUAGE."
  (-distinct (--map (plist-get it :rgtype)
                    (--filter (and
                               (plist-get it :rgtype)
                               (string= (plist-get it :language) language))
                              dumb-jump-language-file-exts))))

(defun dumb-jump-get-git-grep-type-by-language (language)
  "Return list of git grep type argument for a LANGUAGE."
  (-distinct (--map (plist-get it :ext)
                    (--filter (and
                               (plist-get it :ext)
                               (string= (plist-get it :language) language))
                              dumb-jump-language-file-exts))))

(defun dumb-jump-get-rules-by-language (language searcher)
  "Return a list of rules for the LANGUAGE."
  (let* ((searcher-str (cond ((eq 'git-grep searcher) "git-grep")
                             ((eq 'rg searcher) "rg")
                             ((eq 'ag searcher) "ag")
                             (t "grep")))
         (results (--filter (and
                             (string= (plist-get it ':language) language)
                             (member searcher-str (plist-get it ':supports)))
                           dumb-jump-find-rules)))
    (if dumb-jump-functions-only
        (--filter (string= (plist-get it ':type) "function") results)
      results)))

;;;###autoload
(define-minor-mode dumb-jump-mode
  "Minor mode for jumping to variable and function definitions"
  :global t
  :keymap dumb-jump-mode-map)

(provide 'dumb-jump)
;;; dumb-jump.el ends here
