;;; dumb-jump.el --- Jump to definition for 50+ languages without configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2015-2021 jack angers
;; Author: jack angers and contributors
;; Url: https://github.com/jacktasia/dumb-jump
;; Version: 0.5.4
;; Package-Requires: ((emacs "24.4") (s "1.11.0") (dash "2.9.0") (popup "0.5.3"))
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

;; Dumb Jump is an Emacs "jump to definition" package with support for 50+ programming languages that favors
;; "just working" over speed or accuracy.  This means minimal -- and ideally zero -- configuration with absolutely
;; no stored indexes (TAGS) or persistent background processes.
;;
;; Dumb Jump provides a xref-based interface for jumping to
;; definitions.  It is based on tools such as grep, the silver searcher
;; (https://geoff.greer.fm/ag/), ripgrep
;; (https://github.com/BurntSushi/ripgrep) or git-grep
;; (https://git-scm.com/docs/git-grep).
;;
;; To enable Dumb Jump, add the following to your initialisation file:
;;
;;    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;
;; Now pressing M-.  on an identifier should open a buffer at the place
;; where it is defined, or a list of candidates if uncertain.  This
;; list can be navigated using M-g M-n (next-error) and M-g M-p
;; (previous-error).

;;; Code:
(unless (require 'xref nil :noerror)
  (require 'etags))
(require 's)
(require 'dash)
(require 'popup)
(require 'cl-generic nil :noerror)
(require 'cl-lib)
(require 'subr-x)                       ; use: `string-blank-p'

(defgroup dumb-jump nil
  "Easily jump to project function and variable definitions."
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
  "Which window to use when jumping.
Valid options are \\='current (default) or \\='other."
  :group 'dumb-jump
  :type '(choice (const :tag "Current window" current)
                 (const :tag "Other window" other)))

(defcustom dumb-jump-use-visible-window
  t
  "If true will jump in a visible window that shows the opened file."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-selector
  'popup
  "Which selector to use when there is multiple choices.
The available selectors are:
- popup: https://github.com/auto-complete/popup-el
- helm : https://emacs-helm.github.io/helm/
- ivy  : https://github.com/abo-abo/swiper
- Completing Read: Emacs default `completing-read'.

When selecting helm or ivy, if the corresponding package is not installed,
the selector defaults to popup."
  :group 'dumb-jump
  :type '(choice (const :tag "Popup" popup)
                 (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)
                 (const :tag "Completing Read" completing-read)))

(defcustom dumb-jump-ivy-jump-to-selected-function
  #'dumb-jump-ivy-jump-to-selected
  "Prompts user for a choice using ivy then dumb-jump to that choice."
  :group 'dumb-jump
  :type 'function)

(defcustom dumb-jump-prefer-searcher nil
  "The preferred searcher command line tool to use.
The available choices are:
- \\='ag              : https://github.com/ggreer/the_silver_searcher
- \\='rg              : https://github.com/BurntSushi/ripgrep
- \\='grep            : https://en.wikipedia.org/wiki/Grep
- \\='gnu-grep        : https://www.gnu.org/software/grep/manual/grep.html
- \\='git-grep        : https://git-scm.com/docs/git-grep
- \\='git-grep-plus-ag

If nil, select the most optimal searcher at runtime, looking for all
tools listed above except for git-grep in the order of that list.
However, the choice is overridden by `dumb-jump-force-searcher' selection
unless that is nil."
  :group 'dumb-jump
  :type '(choice (const :tag "Best Available" nil)
                 (const :tag "ag" ag)
                 (const :tag "rg" rg)
                 (const :tag "grep" grep)
                 (const :tag "gnu-grep" gnu-grep)
                 (const :tag "git grep" git-grep)
                 (const :tag "git grep + ag" git-grep-plus-ag)))

(defcustom dumb-jump-force-searcher nil
  "Search tool override of `dumb-jump-prefer-searcher' selection.

By default this is nil to honour the choice made by `dumb-jump-prefer-searcher'.

However, you may want to override that choice for some projects by identifying
one of two preferred choice overriding methods:
- Override preferred via user-specified function:
   Enter the name of a function that takes the current directory and return the
   search tool symbol to use for this project or nil if the choice made in
   `dumb-jump-prefer-searcher' must be honoured.
- Override preferred for directories needing git-grep:
   Specify one or more project directories where the git-grep search tool must
   be used.
- Override to a specific tool.  That is mostly useful to set the overriding
  inside the .dir-locals.el of the directory.
  The choices are: \\='ag, \\='rg, \\='grep, \\='gnu-grep, \\='git-grep, or
                   \\='git-grep-plus-ag."
  :group 'dumb-jump
  :type '(choice (const
                  :tag "nil: honour `dumb-jump-prefer-searcher' choice." nil)
                 (function
                  :tag "Override preferred via user-specified function")
                 (repeat
                  :tag "Override preferred for directories needing git-grep"
                  (directory :tag "Repo root"))
                 (const :tag "ag" ag)
                 (const :tag "rg" rg)
                 (const :tag "grep" grep)
                 (const :tag "gnu grep" gnu-grep)
                 (const :tag "git grep" git-grep)
                 (const :tag "git grep + ag" git-grep-plus-ag)))

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
  "The path to the silver searcher.  By default assumes it is in path."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-rg-cmd
  "rg"
  "The path to ripgrep.  By default assumes it is in path."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-git-grep-cmd
  "git grep"
  "The path to git grep.  By default assumes it is in path."
  :group 'dumb-jump
  :type 'string)

;; ---------------------------------------------------------------------------
;; Dumb-jump generic regular expression meta-concepts
;;
;; - `JJJ` : Is replaced by the searched identifier
;; - `\\j` : Represents the word-boundary regular-expression to use instead
;;           of `\\b` when word boundary must not include '-'.
;;           In the Lisp family of programming languages the '-' character is
;;           allowed in identifiers.
;; - `\\s` : for a single white space character.
;;           Replaced by `[[:space:]]` for some search tools as identified by
;;           `dumb-jump-use-space-bracket-exp-for' returning t for a tool variant.

(defcustom dumb-jump-force-using-space-bracket-exp
  (eq system-type 'windows-nt)
  "When t the `\\\\s` in regexp code strings is replaced by `[[:space:]]`.
Otherwise it is left in the regular expression.
Defaults to t under Windows because older gnu grep versions have problem
dealing with `\\\\s` to match end of words due to the 2 bytes used to
identify the end of line."
  :group 'dumb-jump
  :type 'boolean)

(defun dumb-jump-use-space-bracket-exp-for (variant)
  "Return t when `\\\\s` must be replaced by `[[:space]]` for VARIANT.
VARIANT must be a one of the following symbols: ag, rg, grep, gnu-grep,
git-grep or git-grep-plus-ag."
  (and dumb-jump-force-using-space-bracket-exp
       (memq variant '(grep gnu-grep git-grep git-grep-plus-ag))))

;; word-boundary regexps: they replace the "\\j" in dumb-jump regexes.
;; [:todo 2026-01-29, by Pierre Rouleau: Should we not exclude '_' from word boundary?]
(defcustom dumb-jump-ag-word-boundary
  "(?![a-zA-Z0-9\\?\\*-])"
  "Regexp that replaces `\\j` in dumb-jump regexes for ag search.
`\\b` thinks `-` is a word boundary.
When this matters use `\\j` instead and ag will use this value."
  :group 'dumb-jump
  :type 'string)

;; [:todo 2026-01-29, by Pierre Rouleau: Should we not exclude '_' from word boundary?]
(defcustom dumb-jump-rg-word-boundary
  "($|[^a-zA-Z0-9\\?\\*-])"
  "Regexp that replaces `\\j` in dumb-jump regexes for rg search.
`\\b` thinks `-` is a word boundary.
When this matters use `\\j` instead and rg will use this value."
  :group 'dumb-jump
  :type 'string)

;; [:todo 2026-01-29, by Pierre Rouleau: Should we not exclude '_' from word boundary?]
(defcustom dumb-jump-git-grep-word-boundary
  "($|[^a-zA-Z0-9\\?\\*-])"
  "Regexp that replaces `\\j` in dumb-jump regexes for git-grep search.
`\\b` thinks `-` is a word boundary.
When this matters use `\\j` instead and git grep will use this value."
  :group 'dumb-jump
  :type 'string)

;; [:todo 2026-01-29, by Pierre Rouleau: Should we not exclude '_' from word boundary?]
(defcustom dumb-jump-grep-word-boundary
  "($|[^a-zA-Z0-9\\?\\*-])"
  "Regexp that replaces `\\j` in dumb-jump regexes for grep search.
`\\b` thinks `-` is a word boundary.
When this matters use `\\j` instead and grep will use this value."
  :group 'dumb-jump
  :type 'string)

;; [:todo 2026-01-29, by Pierre Rouleau: Should we not exclude '_' from word boundary?]
(defcustom dumb-jump-gnu-grep-word-boundary
  "($|[^a-zA-Z0-9\\?\\*-])"
  "Regexp that replaces `\\j` in dumb-jump regexes for gnu grep search.
`\\b` thinks `-` is a word boundary.
When this matters use `\\j` instead and grep will use this value."
  :group 'dumb-jump
  :type 'string)

;; --

(defcustom dumb-jump-fallback-regex
  "\\bJJJ\\j"
  "When `dumb-jump-fallback-search' is t use this regex.
Defaults to boundary search of symbol under point."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-fallback-search
  t
  "If nothing is found with normal search fallback searching fallback regex."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-zgrep-cmd
  "zgrep"
  "The path to grep to use for gzipped files.
By default assumes it is in path."
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
  "Number of seconds a grep/find command can take before issuing a warning.
The warning recommend using a faster search tool and config."
  :group 'dumb-jump
  :type 'integer)

(defcustom dumb-jump-functions-only
  nil
  "Should we only jump to functions?"
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-quiet
  nil
  "If non-nil Dumb Jump will not log anything to *Messages*.
Note that it prevents printing when `dumb-jump-debug' is non-nil."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-ignore-context
  nil
  "If non-nil Dumb Jump will ignore the context of point when jumping."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-git-grep-search-untracked
  t
  "If non-nil Dumb Jump will also search untracked files when using git-grep."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-git-grep-search-args
  ""
  "Appends the passed arguments to the git-grep search function.
Default: \"\"."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-ag-search-args
  ""
  "Appends the passed arguments to the ag search function.
Default: \"\"."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-rg-search-args
  "--pcre2"
  "Appends the passed arguments to the rg search function.
Default: \"--pcre2\"."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-search-type-org-only-org
  t
  "If non nil restrict type ag/rg to org file.
If nil add also the language type of current src block."
  :group 'dumb-jump
  :type 'boolean)

;; [:todo 2026-01-29, by Pierre Rouleau: Why 3 backslashes in front of '('? ]
(defcustom dumb-jump-find-rules
  ;;-- elisp
  '((:language "elisp" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\((defun|cl-defun|cl-defgeneric|cl-defmethod|cl-defsubst)\\s+JJJ\\j"
           :tests ("(defun test (blah)"
                   "(defun test\n"
                   "(cl-defun test (blah)"
                   "(cl-defun test\n")
           :not ("(defun test-asdf (blah)"
                 "(defun test-blah\n"
                 "(cl-defun test-asdf (blah)"
                 "(cl-defun test-blah\n"
                 "(defun tester (blah)"
                 "(defun test? (blah)"
                 "(defun test- (blah)"))

    (:language "elisp" :type "function" ; macros
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\((defmacro|cl-defmacro|cl-define-compiler-macro)\\s+JJJ\\j"
           :tests ("(defmacro test (blah)"
                   "(defmacro test\n")
           :not ("(defmacro test-asdf (blah)"
                 "(defmacro test-blah\n"
                 "(defmacro tester (blah)"
                 "(defmacro test? (blah)"
                 "(defmacro test- (blah)"))

    (:language "elisp" :type "function" ; hydras
               :supports ("ag" "grep" "rg" "git-grep")
               :regex "\\(defhydra\\b\\s*JJJ\\j"
               :tests ("(defhydra test "
                       "(defhydra test\n"
                       "(defhydra test (blah\n"
                       "(defhydra test (blah)")
               :not ("(defhydra tester"
                     "(defhydra test?"
                     "(defhydra test-"
                     "(defhydra test? (blah\n"
                     "(defhydra test? (blah)"))

    (:language "elisp" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(defvar(-local)?\\b\\s*JJJ\\j"
           :tests ("(defvar test "
                   "(defvar test\n"
                   "(defvar-local test"
                   "(defvar-local test\n")
           :not ("(defvar tester"
                 "(defvar test?"
                 "(defvar test-"
                 "(defvar-local tester"
                 "(defvar-local test?"
                 "(defvar-local test-"))

    (:language "elisp" :type "variable"
           :supports ("ag" "grep" "rg" "ugrep" "git-grep")
           :regex "\\(defconst\\b\\s*JJJ\\j"
           :tests ("(defconst test "
                   "(defconst test\n")
           :not ("(defconst tester"
                 "(defconst test?"
                 "(defconst test-"))

    (:language "elisp" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(defcustom\\b\\s*JJJ\\j"
           :tests ("(defcustom test "
                   "(defcustom test\n")
           :not ("(defcustom tester"
                 "(defcustom test?"
                 "(defcustom test-"))

    (:language "elisp" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(setq\\b\\s*JJJ\\j"
           :tests ("(setq test 123)")
           :not ("setq test-blah 123)"
                 "(setq tester"
                 "(setq test?" "(setq test-"))

    ;; The following regex identifying let-bound variables is the reason why
    ;; searching for an elisp function cause a set of false positive on all
    ;; locations where the function is invoked.
    (:language "elisp" :type "variable" ; let bound variables
               :supports ("ag" "grep" "rg" "git-grep")
               :regex "\\(JJJ\\s+"
               :tests ("(let ((test 123)))"
                       "(let* ((test 123)))")
               :not ("(let ((test-2 123)))"
                     "(let* ((test-2 123)))"))

    (:language "elisp" :type "type"     ; cl-lib structured type definitions
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\((cl-defstruct|cl-deftype)\\s+JJJ\\j"
           :tests ("(cl-defstruct test "
                   "(cl-defstruct test\n"
                   "(cl-deftype test "
                   "(cl-deftype test\n")
           :not ("(cl-defstruct test-asdf (blah)"
                 "(cl-defstruct test-blah\n"
                 "(cl-deftype test-asdf (blah)"
                 "(cl-deftype test-blah\n"))

    (:language "elisp" :type "variable" ;; variable in method signature
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\((defun|cl-defun|cl-defgeneric|cl-defmethod)\\s*.+\\(?\\s*JJJ\\j\\s*\\)?"
           :tests ("(defun blah (test)"
                   "(defun blah (test blah)"
                   "(defun (blah test)")
           :not ("(defun blah (test-1)"
                 "(defun blah (test-2 blah)"
                 "(defun (blah test-3)"))

    ;;-- common lisp
    (:language "commonlisp" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(def(un|macro|generic|method|setf)\\s+JJJ\\j"
           ;; \\j usage see `dumb-jump-ag-word-boundary`
           :tests ("(defun test (blah)"
                   "(defun test\n"
                   "(defmacro test (blah)"
                   "(defmacro test\n"
                   "(defgeneric test (blah)"
                   "(defgeneric test\n"
                   "(defmethod test (blah)"
                   "(defmethod test\n"
                   "(defsetf test (blah)"
                   "(defsetf test\n")
           :not ("(defun test-asdf (blah)"
                 "(defun test-blah\n"
                 "(defun tester (blah)"
                 "(defun test? (blah)"
                 "(defun test- (blah)"
                 "(defmacro test-asdf (blah)"
                 "(defmacro test-blah\n"
                 "(defmacro tester (blah)"
                 "(defmacro test? (blah)"
                 "(defmacro test- (blah)"
                 "(defgeneric test-asdf (blah)"
                 "(defgeneric test-blah\n"
                 "(defgeneric tester (blah)"
                 "(defgeneric test? (blah)"
                 "(defun test- (blah)"
                 "(defmethod test-asdf (blah)"
                 "(defmethod test-blah\n"
                 "(defmethod tester (blah)"
                 "(defmethod test? (blah)"
                 "(defun test- (blah)"
                 "(defsetf test-asdf (blah)"
                 "(defsetf test-blah\n"
                 "(defsetf tester (blah)"
                 "(defsetf test? (blah)"
                 "(defun test- (blah)"))

    (:language "commonlisp" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define-(modify-macro|compiler-macro|setf-expander)\\s+JJJ\\j"
           ;; \\j usage see `dumb-jump-ag-word-boundary`
           :tests ("(define-modify-macro test (blah)"
                   "(define-modify-macro test\n"
                   "(define-compiler-macro test (blah)"
                   "(define-compiler-macro test\n")
           :not ("(define-modify-macro test-asdf (blah)"
                 "(define-modify-macro test-blah\n"
                 "(define-modify-macro tester (blah)"
                 "(define-modify-macro test? (blah)"
                 "(define-modify-macro test- (blah)"
                 "(define-compiler-macro test-asdf (blah)"
                 "(define-compiler-macro test-blah\n"
                 "(define-compiler-macro tester (blah)"
                 "(define-compiler-macro test? (blah)"
                 "(define-compiler-macro test- (blah)"))

    (:language "commonlisp" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(def(var|parameter|constant)\\b\\s*JJJ\\j"
           :tests ("(defvar test "
                   "(defvar test\n"
                   "(defparameter test "
                   "(defparameter test\n"
                   "(defconstant test "
                   "(defconstant test\n")
           :not ("(defvar tester"
                 "(defvar test?"
                 "(defvar test-"
                 "(defparameter tester"
                 "(defparameter test?"
                 "(defparameter test-"
                 "(defconstant tester"
                 "(defconstant test?"
                 "(defconstant test-"))

    (:language "commonlisp" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define-symbol-macro\\b\\s*JJJ\\j"
           :tests ("(define-symbol-macro test "
                   "(define-symbol-macro test\n")
           :not ("(define-symbol-macro tester"
                 "(define-symbol-macro test?"
                 "(define-symbol-macro test-"))

    (:language "commonlisp" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(def(class|struct|type)\\b\\s*JJJ\\j"
           :tests ("(defclass test "
                   "(defclass test\n"
                   "(defstruct test "
                   "(defstruct test\n"
                   "(deftype test "
                   "(deftype test\n")
           :not ("(defclass tester"
                 "(defclass test?"
                 "(defclass test-"
                 "(defstruct tester"
                 "(defstruct test?"
                 "(defstruct test-"
                 "(deftype tester"
                 "(deftype test?"
                 "(deftype test-"))

    (:language "commonlisp" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define-condition\\b\\s*JJJ\\j"
           :tests ("(define-condition test "
                   "(define-condition test\n")
           :not ("(define-condition tester"
                 "(define-condition test?"
                 "(define-condition test-"))

    ;;-- racket
    (:language "racket" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+\\(\\s*JJJ\\j"
           :tests ("(define (test blah)"
                   "(define (test\n")
           :not ("(define test blah"
                 "(define (test-asdf blah)"
                 "(define test (lambda (blah"))

    (:language "racket" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+JJJ\\s*\\\(\\s*lambda"
           :tests ("(define test (lambda (blah"
                   "(define test (lambda\n")
           :not ("(define test blah"
                 "(define test-asdf (lambda (blah)"
                 "(define (test)"
                 "(define (test blah) (lambda (foo"))

    (:language "racket" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(let\\s+JJJ\\s*(\\\(|\\\[)*"
           :tests ("(let test ((blah foo) (bar bas))"
                   "(let test\n"
                   "(let test [(foo")
           :not ("(let ((test blah"))

    (:language "racket" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+JJJ\\j"
           :tests ("(define test " "(define test\n")
           :not ("(define (test"))

    (:language "racket" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(\\\(|\\\[)\\s*JJJ\\s+"
           :tests ("(let ((test 'foo"
                   "(let [(test 'foo"
                   "(let [(test 'foo"
                   "(let [[test 'foo"
                   "(let ((blah 'foo) (test 'bar)")
           :not ("{test foo"))

    (:language "racket" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(lambda\\s+\\\(?[^\(\)]*\\s*JJJ\\j\\s*\\\)?"
           :tests ("(lambda (test)"
                   "(lambda (foo test)"
                   "(lambda test (foo)")
           :not ("(lambda () test"))

    (:language "racket" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+\\\([^\(\)]+\\s*JJJ\\j\\s*\\\)?"
           :tests ("(define (foo test)"
                   "(define (foo test bar)")
           :not ("(define foo test"
                 "(define (test foo"
                 "(define (test)"))

    (:language "racket" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(struct\\s+JJJ\\j"
           :tests ("(struct test (a b)"))

    ;;-- scheme
    (:language "scheme" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+\\(\\s*JJJ\\j"
           :tests ("(define (test blah)"
                   "(define (test\n")
           :not ("(define test blah"
                 "(define (test-asdf blah)"
                 "(define test (lambda (blah"))

    (:language "scheme" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+JJJ\\s*\\\(\\s*lambda"
           :tests ("(define test (lambda (blah"
                   "(define test (lambda\n")
           :not ("(define test blah"
                 "(define test-asdf (lambda (blah)"
                 "(define (test)"
                 "(define (test blah) (lambda (foo"))

    (:language "scheme" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(let\\s+JJJ\\s*(\\\(|\\\[)*"
           :tests ("(let test ((blah foo) (bar bas))"
                   "(let test\n"
                   "(let test [(foo")
           :not ("(let ((test blah"))

    (:language "scheme" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+JJJ\\j"
           :tests ("(define test "
                   "(define test\n")
           :not ("(define (test"))

    (:language "scheme" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(\\\(|\\\[)\\s*JJJ\\s+"
           :tests ("(let ((test 'foo"
                   "(let [(test 'foo"
                   "(let [(test 'foo"
                   "(let [[test 'foo"
                   "(let ((blah 'foo) (test 'bar)")
           :not ("{test foo"))

    (:language "scheme" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(lambda\\s+\\\(?[^\(\)]*\\s*JJJ\\j\\s*\\\)?"
           :tests ("(lambda (test)"
                   "(lambda (foo test)"
                   "(lambda test (foo)")
           :not ("(lambda () test"))

    (:language "scheme" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\(define\\s+\\\([^\(\)]+\\s*JJJ\\j\\s*\\\)?"
           :tests ("(define (foo test)"
                   "(define (foo test bar)")
           :not ("(define foo test"
                 "(define (test foo"
                 "(define (test)"))

    ;;-- janet
    (:language "janet" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(\(de\)?f\\s+JJJ\\j"
           :tests ("(def test (foo)"))

    (:language "janet" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(var\\s+JJJ\\j"
           :tests ("(var test (foo)"))

    (:language "janet" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(\(de\)fn-?\\s+JJJ\\j"
           :tests ("(defn test [foo]"
                   "(defn- test [foo]")
           :not ("(defn test? [foo]"
                 "(defn- test? [foo]"))

    (:language "janet" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(defmacro\\s+JJJ\\j"
           :tests ("(defmacro test [foo]"))

    ;;-- c++
    (:language "c++" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\bJJJ(\\s|\\))*\\((\\w|[,&*.<>:]|\\s)*(\\))\\s*(const|->|\\{|$)|typedef\\s+(\\w|[(*]|\\s)+JJJ(\\)|\\s)*\\("
           :tests ("int test(){"
                   "my_struct (*test)(int a, int b){"
                   "auto MyClass::test ( Builder::Builder& reference, ) -> decltype( builder.func() ) {"
                   "int test( int *random_argument) const {"
                   "test::test() {"
                   "typedef int (*test)(int);")
           :not ("return test();)"
                 "int test(a, b);"
                 "if( test() ) {"
                 "else test();"))

    ;; (:language "c++" :type "variable"
    ;;        :supports ("grep")
    ;;        :regex "(\\b\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[([0-9]|\\s)*\\])*\\s*([=,){;]|:\\s*[0-9])|#define\\s+JJJ\\b"
    ;;        :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "unsigned int test:2;"))

    (:language "c++" :type "variable"
           :supports ("ag" "rg")
           :regex "\\b(?!(class\\b|struct\\b|return\\b|else\\b|delete\\b))(\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[(\\d|\\s)*\\])*\\s*([=,(){;]|:\\s*\\d)|#define\\s+JJJ\\b"
           :tests ("int test=2;"
                   "char *test;"
                   "int x = 1, test = 2"
                   "int test[20];"
                   "#define test"
                   "typedef int test;"
                   "unsigned int test:2")
           :not ("return test;"
                 "#define NOT test"
                 "else test=2;"))

    (:language "c++" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "\\b(class|struct|enum|union)\\b\\s*JJJ\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*JJJ\\b\\s*;"
           :tests ("typedef struct test {"
                   "enum test {"
                   "} test;"
                   "union test {"
                   "class test final: public Parent1, private Parent2{"
                   "class test : public std::vector<int> {")
           :not("union test var;"
                "struct test function() {"))

    ;;-- clojure
    (:language "clojure" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
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

    ;;-- coffeescript
    (:language "coffeescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*JJJ\\s*[=:].*[-=]>"
           :tests ("test = ()  =>"
                   "test= =>"
                   "test = ->"
                   "test=()->"
                   "test : ()  =>"
                   "test: =>"
                   "test : ->"
                   "test:()->")
           :not ("# test = =>"
                 "test = 1"))

    (:language "coffeescript" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*JJJ\\s*[:=][^:=-][^>]+$"
           :tests ("test = $"
                   "test : ["
                   "test = {"
                   "test = a")
           :not ("test::a"
                 "test: =>"
                 "test == 1"
                 "# test = 1"))

    (:language "coffeescript" :type "class"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*\\bclass\\s+JJJ"
           :tests ("class test"
                   "class test extends")
           :not ("# class"))

    ;;-- objc : Objective-C
    (:language "objc" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\)\\s*JJJ(:|\\b|\\s)"
           :tests ("- (void)test"
                   "- (void)test:(UIAlertView *)alertView")
           :not ("- (void)testnot"
                 "- (void)testnot:(UIAlertView *)alertView"))

    (:language "objc" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\b\\*?JJJ\\s*=[^=\\n]+"
           :tests ("NSString *test = @\"asdf\"")
           :not ("NSString *testnot = @\"asdf\""
                 "NSString *nottest = @\"asdf\""))

    (:language "objc" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(@interface|@protocol|@implementation)\\b\\s*JJJ\\b\\s*"
           :tests ("@interface test: UIWindow")
           :not ("@interface testnon: UIWindow"))


    (:language "objc" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "typedef\\b\\s+(NS_OPTIONS|NS_ENUM)\\b\\([^,]+?,\\s*JJJ\\b\\s*"
           :tests ("typedef NS_ENUM(NSUInteger, test)")
           :not ("typedef NS_ENUMD(NSUInteger, test)"))

    ;;-- swift
    (:language "swift" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(let|var)\\s*JJJ\\s*(=|:)[^=:\\n]+"
           :tests ("let test = 1234"
                   "var test = 1234"
                   "private lazy var test: UITapGestureRecognizer")
           :not ("if test == 1234:"))

    (:language "swift" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "func\\s+JJJ\\b\\s*(<[^>]*>)?\\s*\\("
           :tests ("func test(asdf)"
                   "func test()"
                   "func test<Value: Protocol>()")
           :not ("func testnot(asdf)"
                 "func testnot()"))

    (:language "swift" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(class|struct|protocol|enum)\\s+JJJ\\b\\s*?"
           :tests ("struct test"
                   "struct test: Codable"
                   "struct test<Value: Codable>"
                   "class test:"
                   "class test: UIWindow"
                   "class test<Value: Codable>")
           :not ("class testnot:"
                 "class testnot(object):"
                 "struct testnot(object)"))

    (:language "swift" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(typealias)\\s+JJJ\\b\\s*?="
           :tests ("typealias test =")
           :not ("typealias testnot"))

    ;;-- csharp : C#
    (:language "csharp" :type "function"
           :supports ("ag" "rg")
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()"
                   "int test(param)"
                   "static int test()"
                   "static int test(param)"
                   "public static MyType test()"
                   "private virtual SomeType test(param)"
                   "static int test()")
           :not ("test()"
                 "testnot()"
                 "blah = new test()"))

    (:language "csharp" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+"
           :tests ("int test = 1234")
           :not ("if test == 1234:"
                 "int nottest = 44"))

    (:type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :language "csharp"
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:"
                   "public class test : IReadableChannel, I")
           :not ("class testnot:"
                 "public class testnot : IReadableChannel, I"))

    ;;-- java (literally the same regexes as c#, but different tests)
    (:language "java" :type "function"
           :supports ("ag" "rg")
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()"
                   "int test(param)"
                   "static int test()"
                   "static int test(param)"
                   "public static MyType test()"
                   "private virtual SomeType test(param)"
                   "static int test()"
                   "private foo[] test()")
           :not ("test()"
                 "testnot()"
                 "blah = new test()"
                 "foo bar = test()"))

    (:language "java" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+"
           :tests ("int test = 1234")
           :not ("if test == 1234:"
                 "int nottest = 44"))

    (:language "java" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:"
                   "public class test implements Something")
           :not ("class testnot:"
                 "public class testnot implements Something"))

    ;;-- vala (again just like c#, exactly the same..)
    (:language "vala" :type "function"
           :supports ("ag" "rg")
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()"
                   "int test(param)"
                   "static int test()"
                   "static int test(param)"
                   "public static MyType test()"
                   "private virtual SomeType test(param)"
                   "static int test()")
           :not ("test()"
                 "testnot()"
                 "blah = new test()"))

    (:language "vala" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+"
           :tests ("int test = 1234")
           :not ("if test == 1234:"
                 "int nottest = 44"))

    (:language "vala" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:"
                   "public class test : IReadableChannel, I")
           :not ("class testnot:"
                 "public class testnot : IReadableChannel, I"))

    ;;-- coq (renamed Rocq in March 2025)
    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Variable\\s+JJJ\\b"
           :tests ("Variable test")
           :not ("Variable testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Inductive\\s+JJJ\\b"
           :tests ("Inductive test")
           :not ("Inductive testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Lemma\\s+JJJ\\b"
           :tests ("Lemma test")
           :not ("Lemma testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Definition\\s+JJJ\\b"
           :tests ("Definition test")
           :not ("Definition testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Hypothesis\\s+JJJ\\b"
           :tests ("Hypothesis test")
           :not ("Hypothesis testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Theorm\\s+JJJ\\b"
           :tests ("Theorm test")
           :not ("Theorm testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Fixpoint\\s+JJJ\\b"
           :tests ("Fixpoint test")
           :not ("Fixpoint testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*Module\\s+JJJ\\b"
           :tests ("Module test")
           :not ("Module testx"))

    (:language "coq" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "\\s*CoInductive\\s+JJJ\\b"
           :tests ("CoInductive test")
           :not ("CoInductive testx"))

    ;;-- python
    (:language "python" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234:"
                 "_test = 1234"))

    (:language "python" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "def\\s*JJJ\\b\\s*\\\("
           :tests ("\tdef test(asdf)"
                   "def test()")
           :not ("\tdef testnot(asdf)"
                 "def testnot()"))

    (:language "python" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\b\\s*\\\(?"
           :tests ("class test(object):"
                   "class test:")
           :not ("class testnot:"
                 "class testnot(object):"))

    ;;-- matlab
    (:language "matlab" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("for test = 1:2:"
                 "_test = 1234"))

    (:language "matlab" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*function\\s*[^=]+\\s*=\\s*JJJ\\b"
           :tests ("\tfunction y = test(asdf)"
                   "function x = test()"
                   "function [x, losses] = test(A, y, lambda, method, qtile)")
           :not ("\tfunction testnot(asdf)"
                 "function testnot()"))

    (:language "matlab" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*classdef\\s*JJJ\\b\\s*"
           :tests ("classdef test")
           :not ("classdef testnot"))

    ;;-- nim
    (:language "nim" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(const|let|var)\\s*JJJ\\*?\\s*(=|:)[^=:\\n]+"
           :tests ("let test = 1234"
                   "var test = 1234"
                   "var test: Stat"
                   "const test = 1234"
                   "const test* = 1234")
           :not ("if test == 1234:"))

    (:language "nim" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(proc|func|macro|template)\\s*`?JJJ`?\\b\\*?\\s*\\\("
           :tests ("\tproc test(asdf)"
                   "proc test()"
                   "func test()"
                   "macro test()"
                   "template test()"
                   "proc test*()")
           :not ("\tproc testnot(asdf)"
                 "proc testnot()"))

    (:language "nim" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "type\\s*JJJ\\b\\*?\\s*(\\{[^}]+\\})?\\s*=\\s*\\w+"
           :tests ("type test = object"
                   "type test {.pure.} = enum"
                   "type test* = ref object")
           :not ("type testnot = object"))

    ;;-- nix
    (:language "nix" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\b\\s*JJJ\\s*=[^=;]+"
           :tests ("test = 1234;"
                   "test = 123;"
                   "test=123")
           :not ("testNot = 1234;"
                 "Nottest = 1234;"
                 "AtestNot = 1234;"))

    ;;-- org
    ;; org named blocks
    (:language "org" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*#\\+[nN][aA][mM][eE]:\\s*JJJ\\j"
           :tests ("#+name: test"
                   "#+NAME: test"
                   "#+Name: test"
                   "  #+name: test"
                   "#+name:test")
           :not ("#+name: tester"
                 "#+name: test-block"
                 "some #+name: test"))

    ;; org call references (#+call: block-name)
    (:language "org" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*#\\+[cC][aA][lL][lL]:\\s*JJJ\\j"
           :tests ("#+call: test"
                   "#+CALL: test"
                   "#+Call: test"
                   "  #+call: test"
                   "#+call: test()"
                   "#+call: test(x=1)")
           :not ("#+call: tester"
                 "#+call: test-other"
                 "some #+call: test"))

    ;; org headings (* Heading text)
    (:language "org" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\*+\\s+JJJ\\j"
           :tests ("* test"
                   "** test"
                   "*** test"
                   "**** test"
                   "* test heading"
                   "** test with more words")
           :not ("* tester"
                 "** testing"
                 "* test-other"
                 "not a * test heading"))

    ;; org custom ID (:CUSTOM_ID: value in property drawers)
    (:language "org" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*:CUSTOM_ID:\\s*JJJ\\j"
           :tests (":CUSTOM_ID: test"
                   ":CUSTOM_ID:  test"
                   "  :CUSTOM_ID: test"
                   ":CUSTOM_ID:test")
           :not (":CUSTOM_ID: tester"
                 ":CUSTOM_ID: test-other"
                 "inline :CUSTOM_ID: test text"))

    ;;-- ruby
    (:language "ruby" :type "variable"
           :supports ("ag" "rg" "git-grep")
           :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
           :tests ("test = 1234"
                   "self.foo, test, bar = args")
           :not ("if test == 1234"
                 "foo_test = 1234"))

    (:language "ruby" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
           :tests ("def test(foo)"
                   "def test()"
                   "def test foo"
                   "def test; end"
                   "def self.test()"
                   "def MODULE::test()"
                   "private def test")
           :not ("def test_foo"))

    (:language "ruby" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|\\W)define(_singleton|_instance)?_method(\\s|[(])\\s*:JJJ($|[^\\w|:])"
           :tests ("define_method(:test, &body)"
                   "mod.define_instance_method(:test) { body }"))

    (:language "ruby" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("class test"
                   "class Foo::test"))

    (:language "ruby" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])module\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("module test"
                   "module Foo::test"))

    (:language "ruby" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|\\W)alias(_method)?\\W+JJJ(\\W|$)"
           :tests ("alias test some_method"
                   "alias_method :test, :some_method"
                   "alias_method 'test' 'some_method'"
                   "some_class.send(:alias_method, :test, :some_method)")
           :not ("alias some_method test"
                 "alias_method :some_method, :test"
                 "alias test_foo test"))

    ;;-- Groovy
    (:language "groovy" :type "variable"
           :supports ("ag" "rg" "git-grep")
           :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
           :tests ("test = 1234"
                   "self.foo, test, bar = args")
           :not ("if test == 1234"
                 "foo_test = 1234"))

    (:language "groovy" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])((private|public)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
           :tests ("def test(foo)"
                   "def test()"
                   "def test foo"
                   "def test; end"
                   "def self.test()"
                   "def MODULE::test()"
                   "private def test")
           :not ("def test_foo"))

    (:language "groovy" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("class test"
                   "class Foo::test"))

    ;;-- crystal
    (:language "crystal" :type "variable"
           :supports ("ag" "rg" "git-grep")
           :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
           :tests ("test = 1234"
                   "self.foo, test, bar = args")
           :not ("if test == 1234"
                 "foo_test = 1234"))

    (:language "crystal" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
           :tests ("def test(foo)"
                   "def test()"
                   "def test foo"
                   "def test; end"
                   "def self.test()"
                   "def MODULE::test()"
                   "private def test")
           :not ("def test_foo"))

    (:language "crystal" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("class test"
                   "class Foo::test"))

    (:language "crystal" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])module\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("module test"
                   "module Foo::test"))

    (:language "crystal" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])struct\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("struct test"
                   "struct Foo::test"))

    (:language "crystal" :type "type"
           :supports ("ag" "rg" "git-grep")
           :regex "(^|[^\\w.])alias\\s+(\\w*::)*JJJ($|[^\\w|:])"
           :tests ("alias test"
                   "alias Foo::test"))

    ;;-- scad
    (:language "scad" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234 {"))

    (:language "scad" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()"
                   "function test ()"))

    (:language "scad" :type "module"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "module\\s*JJJ\\s*\\\("
           :tests ("module test()"
                   "module test ()"))

    ;;-- scala
    (:language "scala" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bval\\s*JJJ\\s*=[^=\\n]+"
           :tests ("val test = 1234")
           :not ("case test => 1234"))

    (:language "scala" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bvar\\s*JJJ\\s*=[^=\\n]+"
           :tests ("var test = 1234")
           :not ("case test => 1234"))

    (:language "scala" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\btype\\s*JJJ\\s*=[^=\\n]+"
           :tests ("type test = 1234")
           :not ("case test => 1234"))

    (:language "scala" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bdef\\s*JJJ\\s*\\\("
           :tests ("def test(asdf)" "def test()"))

    (:language "scala" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s*\\\(?"
           :tests ("class test(object)"))

    (:language "scala" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "trait\\s*JJJ\\s*\\\(?"
           :tests ("trait test(object)"))

    (:language "scala" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "object\\s*JJJ\\s*\\\(?"
           :tests ("object test(object)"))

    ;;-- solidity
    (:language "solidity" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex  "function\\s*JJJ\\s*\\\("
           :tests ("function test() internal"
                   "function test (uint x, address y)"
                   "function test() external"))

    (:language "solidity" :type "modifier"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex  "modifier\\s*JJJ\\s*\\\("
           :tests ("modifier test()"
                   "modifier test ()"))

    (:language "solidity" :type "event"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex  "event\\s*JJJ\\s*\\\("
           :tests ("event test();"
                   "event test (uint indexed x)"
                   "event test(uint x, address y)"))

    (:language "solidity" :type "error"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex  "error\\s*JJJ\\s*\\\("
           :tests ("error test();"
                   "error test (uint x)"
                   "error test(uint x, address y)"))

    (:language "solidity" :type "contract"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex  "contract\\s*JJJ\\s*(is|\\\{)"
           :tests ("contract test{"
                   "contract test {"
                   "contract test is foo"))

    ;;-- R
    (:language "r" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*=[^=><]"
           :tests ("test = 1234")
           :not ("if (test == 1234)"))

    (:language "r" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*<-\\s*function\\b"
           :tests ("test <- function"
                   "test <- function(")
           :not   ("test <- functionX"))

    ;;-- perl
    (:language "perl" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "sub\\s*JJJ\\s*(\\{|\\()"
           :tests ("sub test{"
                   "sub test {"
                   "sub test(" "sub test ("))

    (:language "perl" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "JJJ\\s*=\\s*"
           :tests ("$test = 1234"))

    ;;-- Tcl
    (:language "tcl" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "proc\\s+JJJ\\s*\\{"
           :tests ("proc test{" "proc test {"))

    (:language "tcl" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "set\\s+JJJ"
           :tests ("set test 1234"))

    (:language "tcl" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(variable|global)\\s+JJJ"
           :tests ("variable test" "global test"))

    ;;-- shell
    (:language "shell" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*JJJ\\s*"
           :tests ("function test{"
                   "function test {"
                   "function test () {")
           :not   ("function nottest {"))

    (:language "shell" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "JJJ\\\(\\\)\\s*\\{"
           :tests ("test() {")
           :not ("testx() {"))

    (:language "shell" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*=\\s*"
           :tests ("test = 1234")
           :not ("blahtest = 1234"))

    ;;-- php
    (:language "php" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()"
                   "function test ()"))

    (:language "php" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\*\\s@method\\s+[^ \t]+\\s+JJJ\\("
           :tests ("/** @method string|false test($a)"
                   " * @method bool test()"))

    (:language "php" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(\\s|->|\\$|::)JJJ\\s*=\\s*"
           :tests ("$test = 1234"
                   "$foo->test = 1234"))

    (:language "php" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\*\\s@property(-read|-write)?\\s+([^ \t]+\\s+)&?\\$JJJ(\\s+|$)"
           :tests ("/** @property string $test"
                   "/** @property string $test description for $test property"
                   " * @property-read bool|bool $test"
                   " * @property-write \\ArrayObject<string,resource[]> $test"))

    (:language "php" :type "trait"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "trait\\s*JJJ\\s*\\\{"
           :tests ("trait test{"
                   "trait test {"))

    (:language "php" :type "interface"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "interface\\s*JJJ\\s*(extends|\\\{)"
           :tests ("interface test{"
                   "interface test {")
                   "interface test extends test2")

    (:language "php" :type "class"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s*(extends|implements|\\\{)"
           :tests ("class test{"
                   "class test {"
                   "class test extends foo"
                   "class test implements foo"))

    ;; PHP 8.1+ enum definition
    (:language "php" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\benum\\s+JJJ\\b\\s*(:\\s*(int|string))?\\s*(implements[^{]+)?\\{"
           :tests ("enum test {"
                   "enum test: int {"
                   "enum test: string {"
                   "enum test implements Stringable {"
                   "enum test: int implements JsonSerializable {")
           :not ("// enum test" "$enum = 'test'"))

    ;; PHP 8.1+ enum case
    (:language "php" :type "constant"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*case\\s+JJJ\\s*(;|=)"
           :tests ("    case test;"
                   "case test = 1;"
                   "case test = 'value';")
           :not ("case 'test':" "case \"test\":" "default:"))

    ;; PHP abstract class
    (:language "php" :type "class"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\babstract\\s+class\\s+JJJ\\s*(extends|implements|\\\{)"
           :tests ("abstract class test {"
                   "abstract class test extends Base {"
                   "abstract class test implements Foo {")
           :not ("// abstract class test"))

    ;; PHP final class (including final readonly)
    (:language "php" :type "class"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfinal\\s+(readonly\\s+)?class\\s+JJJ\\s*(extends|implements|\\\{)"
           :tests ("final class test {"
                   "final readonly class test {"
                   "final class test extends Base {")
           :not ("// final class test"))

    ;; PHP readonly class (including readonly final)
    (:language "php" :type "class"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\breadonly\\s+(final\\s+)?class\\s+JJJ\\s*(extends|implements|\\\{)"
           :tests ("readonly class test {"
                   "readonly final class test {"
                   "readonly class test implements Serializable {")
           :not ("// readonly class test"))

    ;; PHP class constant with optional visibility/final
    (:language "php" :type "constant"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(final\\s+)?(public|protected|private)?\\s*const\\s+JJJ\\s*="
           :tests ("const test = 'value';"
                   "public const test = 100;"
                   "private const test = 'secret';"
                   "protected const test = [];"
                   "final public const test = 42;"
                   "final const test = true;")
           :not ("$const = 'test'" "// const test"))

    ;; PHP global const (outside class)
    (:language "php" :type "constant"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^const\\s+JJJ\\s*="
           :tests ("const test = 'app_name';"
                   "const test = 42;"
                   "const test = [];")
           :not ("    const test =" "public const test ="))

    ;; PHP define() function
    (:language "php" :type "constant"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*define\\s*\\(\\s*['\"]JJJ['\"]"
           :tests ("define('test', 100);"
                   "define(\"test\", 'value');"
                   "define('test',")
           :not ("// define('test'" "$define = 'test'"))

    ;; PHP typed property declaration
    (:language "php" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(public|protected|private)\\s+(static\\s+)?(readonly\\s+)?\\??[A-Za-z_][A-Za-z0-9_\\\\|&]*\\s+\\$JJJ\\s*(;|=|,)"
           :tests ("public int $test;"
                   "private string $test = '';"
                   "protected ?array $test = null;"
                   "public readonly string $test;"
                   "private static int $test = 0;"
                   "public DateTime $test;")
           :not ("public function foo(int $test)" "return $test;"))

    ;; PHP static property (without explicit type)
    (:language "php" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(public|protected|private)?\\s*static\\s+\\$JJJ\\s*(;|=)"
           :tests ("static $test;"
                   "public static $test = [];"
                   "private static $test = null;")
           :not ("static function test()" "static::$test"))

    ;; PHP namespace declaration
    (:language "php" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bnamespace\\s+([A-Za-z_][A-Za-z0-9_]*\\\\)*JJJ\\s*[;{]"
           :tests ("namespace test;"
                   "namespace App\\test;"
                   "namespace App\\Services\\test {")
           :not ("// namespace test" "use App\\test;"))

    ;; PHP use statement for class imports
    (:language "php" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\buse\\s+([A-Za-z_][A-Za-z0-9_]*\\\\)*JJJ\\s*(;|,|\\s+as\\s+)"
           :tests ("use test;"
                   "use App\\test;"
                   "use App\\Models\\test as TestModel;"
                   "use App\\test, App\\Other;")
           :not ("use function test;" "use const test;"))

    ;;-- dart
    (:language "dart" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
           :tests ("test(foo) {"
                   "test (foo){"
                   "test(foo){"))

    (:language "dart" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test(object) {"
                   "class test{"))

    ;;-- faust
    (:language "faust" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\(\\\(.+\\\)\)*\\s*="
           :tests ("test = osc + 0.5;"
                   "test(freq) = osc(freq) + 0.5;"))

    ;;-- fennel
    (:language "fennel" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\((local|var)\\s+JJJ\\j"
           :tests ("(local test (foo)"
                   "(var test (foo)"))

    (:language "fennel" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(fn\\s+JJJ\\j"
           :tests ("(fn test [foo]")
           :not ("(fn test? [foo]"))

    (:language "fennel" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\(macro\\s+JJJ\\j"
           :tests ("(macro test [foo]"))

    ;;-- fortran
    (:language "fortran" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if (test == 1234)"))

    (:language "fortran" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\b(function|subroutine|FUNCTION|SUBROUTINE)\\s+JJJ\\b\\s*\\\("
           :tests ("function test (foo)"
                   "integer function test(foo)"
                   "subroutine test (foo, bar)"
                   "FUNCTION test (foo)"
                   "INTEGER FUNCTION test(foo)"
                   "SUBROUTINE test (foo, bar)")
           :not ("end function test"
                 "end subroutine test"
                 "END FUNCTION test"
                 "END SUBROUTINE test"))

    (:language "fortran" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*(interface|INTERFACE)\\s+JJJ\\b"
           :tests ("interface test"
                   "INTERFACE test")
           :not ("interface test2"
                 "end interface test"
                 "INTERFACE test2"
                 "END INTERFACE test"))

    (:language "fortran" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^\\s*(module|MODULE)\\s+JJJ\\s*"
           :tests ("module test"
                   "MODULE test")
           :not ("end module test"
                 "END MODULE test"))

    ;;-- go
    (:language "go" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234 {"))

    (:language "go" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*:=\\s*"
           :tests ("test := 1234"))

    (:language "go" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "func\\s+\\\([^\\\)]*\\\)\\s+JJJ\\s*\\\("
           :tests ("func (s *blah) test(filename string) string {"))

    (:language "go" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "func\\s+JJJ\\s*\\\("
           :tests ("func test(url string) (string, error)"))

    (:language "go" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "type\\s+JJJ\\s+struct\\s+\\\{"
           :tests ("type test struct {"))

    ;;-- javascript extended
    (:language "javascript" :tags ("angular") :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(service|factory)\\\(['\"]JJJ['\"]"
           :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

    (:language "javascript" :tags ("es6") :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>"
           :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

    (:language "javascript" :tags ("es6") :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
           :tests ("test(foo) {" "test (foo){" "test(foo){")
           :not ("test = blah.then(function(){"))

    (:language "javascript" :tags ("es6") :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test(object) {" "class test{"))

    (:language "javascript" :tags ("es6") :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s+extends"
           :tests ("class test extends Component{"))

    ;;-- javascript
    (:language "javascript" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234"
                   "const test = props =>")
           :not ("if (test === 1234)"))

    (:language "javascript" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)"
                   "function (test, blah)"
                   "function somefunc(test, blah) {"
                   "function(blah, test)")
           :not ("function (testLen)"
                 "function (test1, blah)"
                 "function somefunc(testFirst, blah) {"
                 "function(blah, testLast)"
                 "function (Lentest)"
                 "function (blahtest, blah)"
                 "function somefunc(Firsttest, blah) {"
                 "function(blah, Lasttest)"))

    (:language "javascript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()"
                   "function test ()"))

    (:language "javascript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
           :tests ("test: function()"))

    (:language "javascript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    ;;-- hcl terraform
    (:language "hcl" :type "block"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(variable|output|module)\\s*\"JJJ\"\\s*\\\{"
           :tests ("variable \"test\" {"
                   "output \"test\" {"
                   "module \"test\" {"))

    (:language "hcl" :type "block"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(data|resource)\\s*\"\\w+\"\\s*\"JJJ\"\\s*\\\{"
           :tests ("data \"openstack_images_image_v2\" \"test\" {"
                   "resource \"google_compute_instance\" \"test\" {"))

    ;;-- typescript
    (:language "typescript" :tags ("angular") :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(service|factory)\\\(['\"]JJJ['\"]"
           :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>"
           :tests ("const test = (foo) => "
                   "test: (foo) => {"
                   "  test: (foo) => {"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
           :tests ("test(foo) {"
                   "test (foo){"
                   "test(foo){")
           :not ("test = blah.then(function(){"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s*[\\\(\\\{]"
           :tests ("class test{"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "class\\s*JJJ\\s+extends"
           :tests ("class test extends Component{"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()"
                   "function test ()"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
           :tests ("test: function()"))

    (:language "typescript" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    (:language "typescript" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234" "const test = props =>")
           :not ("if (test === 1234)"))

    (:language "typescript" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)"
                   "function (test, blah)"
                   "function somefunc(test, blah) {"
                   "function(blah, test)")
           :not ("function (testLen)"
                 "function (test1, blah)"
                 "function somefunc(testFirst, blah) {"
                 "function(blah, testLast)"
                 "function (Lentest)"
                 "function (blahtest, blah)"
                 "function somefunc(Firsttest, blah) {"
                 "function(blah, Lasttest)"))

    ;;-- julia
    (:language "julia" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(@noinline|@inline)?\\s*function\\s*JJJ(\\{[^\\}]*\\})?\\("
           :tests ("function test()"
                   "@inline function test()"
                   "function test{T}(h)"))

    (:language "julia" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(@noinline|@inline)?JJJ(\\{[^\\}]*\\})?\\([^\\)]*\\)\s*="
           :tests ("test(a)=1"
                   "test(a,b)=1*8"
                   "@noinline test()=1"
                   "test{T}(x)=x"))

    (:language "julia" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "macro\\s*JJJ\\("
           :tests ("macro test(a)=1"
                   " macro test(a,b)=1*8"))

    (:language "julia" :type "variable"
           :supports ("ag" "rg")
           :regex "const\\s+JJJ\\b"
           :tests ("const test = "))

    (:language "julia" :type "type"
           :supports ("ag" "rg")
           :regex "(mutable)?\\s*struct\\s*JJJ"
           :tests ("struct test"))

    (:language "julia" :type "type"
           :supports ("ag" "rg")
           :regex "(type|immutable|abstract)\\s*JJJ"
           :tests ("type test"
                   "immutable test"
                   "abstract test <:Testable" ))

    ;;-- haskell
    (:language "haskell" :type "module"
           :supports ("ag")
           :regex "^module\\s+JJJ\\s+"
           :tests ("module Test (exportA, exportB) where"))

                                        ; TODO Doesn't support any '=' in arguments. E.g. 'foo A{a = b,..} = bar'.
    (:language "haskell" :type "top level function"
           :supports ("ag")
           :regex "^\\bJJJ(?!(\\s+::))\\s+((.|\\s)*?)=\\s+"
           :tests ("test n = n * 2"
                   "test X{..} (Y a b c) \n bcd \n =\n x * y"
                   "test ab cd e@Datatype {..} (Another thing, inTheRow) = \n undefined"
                   "test = runRealBasedMode @ext @ctx identity identity"
                   "test unwrap wrap nr@Naoeu {..} (Action action, specSpecs) = \n    undefined")
           :not ("nottest n = n * 2"
                 "let testnot x y = x * y"
                 "test $ y z"
                 "let test a o = mda"
                 "test :: Sometype -> AnotherType aoeu kek = undefined"))

    (:language "haskell" :type "type-like"
           :supports ("ag")
           :regex "^\\s*((data(\\s+family)?)|(newtype)|(type(\\s+family)?))\\s+JJJ\\s+"
           :tests ("newtype Test a = Something { b :: Kek }"
                   "data Test a b = Somecase a | Othercase b"
                   "type family Test (x :: *) (xs :: [*]) :: Nat where"
                   "data family Test "
                   "type Test = TestAlias")
           :not ("newtype NotTest a = NotTest (Not a)"
                 "data TestNot b = Aoeu"))

                                        ; datatype contstuctor that doesn't match type definition.
    (:language "haskell" :type "(data)type constructor 1"
           :supports ("ag")
           :regex "(data|newtype)\\s{1,3}(?!JJJ\\s+)([^=]{1,40})=((\\s{0,3}JJJ\\s+)|([^=]{0,500}?((?<!(-- ))\\|\\s{0,3}JJJ\\s+)))"
           :tests ("data Something a = Test { b :: Kek }"
                   "data Mem a = TrueMem { b :: Kek } | Test (Mem Int) deriving Mda"
                   "newtype SafeTest a = Test (Kek a) deriving (YonedaEmbedding)")
           :not ("data Test = Test { b :: Kek }"))


    (:language "haskell" :type "data/newtype record field"
           :supports ("ag")
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

    (:language "haskell" :type "typeclass"
           :supports ("ag")
           :regex "^class\\s+(.+=>\\s*)?JJJ\\s+"
           :tests (
                   "class (Constr1 m, Constr 2) => Test (Kek a) where"
                   "class  Test  (Veryovka a)  where ")
           :not ("class Test2 (Kek a) where"
                 "class MakeTest (AoeuTest x y z) where"))

    ;;-- ocaml
    (:language "ocaml" :type "type"
           :supports ("ag" "rg")
           :regex "^\\s*(and|type)\\s+.*\\bJJJ\\b"
           :tests ("type test ="
                   "and test ="
                   "type 'a test ="
                   "type ('a, _, 'c) test"))

    (:language "ocaml" :type "variable"
           :supports ("ag" "rg")
           :regex "let\\s+JJJ\\b"
           :tests ("let test ="
                   "let test x y ="))

    (:language "ocaml" :type "variable"
           :supports ("ag" "rg")
           :regex "let\\s+rec\\s+JJJ\\b"
           :tests ("let rec test ="
                   "let rec  test x y ="))

    (:language "ocaml" :type "variable"
           :supports ("ag" "rg")
           :regex "\\s*val\\s*\\bJJJ\\b\\s*"
           :tests ("val test"))

    (:language "ocaml" :type "module"
           :supports ("ag" "rg")
           :regex "^\\s*module\\s*\\bJJJ\\b"
           :tests ("module test ="))

    (:language "ocaml" :type "module"
           :supports ("ag" "rg")
           :regex "^\\s*module\\s*type\\s*\\bJJJ\\b"
           :tests ("module type test ="))

    ;; lua
    (:language "lua" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test === 1234"))

    (:language "lua" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
           :tests ("function (test)"
                   "function (test, blah)"
                   "function somefunc(test, blah)"
                   "function(blah, test)")
           :not ("function (testLen)"
                 "function (test1, blah)"
                 "function somefunc(testFirst, blah)"
                 "function(blah, testLast)"
                 "function (Lentest)"
                 "function (blahtest, blah)"
                 "function somefunc(Firsttest, blah)"
                 "function(blah, Lasttest)"))

    (:language "lua" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))

    (:language "lua" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*.+[.:]JJJ\\s*\\\("
           :tests ("function MyClass.test()"
                   "function MyClass.test ()"
                   "function MyClass:test()"
                   "function MyClass:test ()"))

    (:language "lua" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()"))

    (:language "lua" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\b.+\\.JJJ\\s*=\\s*function\\s*\\\("
           :tests ("MyClass.test = function()"))

    ;;-- rust
    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\blet\\s+(\\\([^=\\n]*)?(mut\s+)?JJJ([^=\\n]*\\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+"
           :tests ("let test = 1234;"
                   "let test: u32 = 1234;"
                   "let test: Vec<u32> = Vec::new();"
                   "let mut test = 1234;"
                   "let mut test: Vec<u32> = Vec::new();"
                   "let (a, test, b) = (1, 2, 3);"
                   "let (a, mut test, mut b) = (1, 2, 3);"
                   "let (mut a, mut test): (u32, usize) = (1, 2);"))

    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bconst\\s+JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
           :tests ("const test: u32 = 1234;"))

    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bstatic\\s+(mut\\s+)?JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
           :tests ("static test: u32 = 1234;"
                   "static mut test: u32 = 1234;"))

    ;; variable in method signature
    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfn\\s+.+\\s*\\\((.+,\\s+)?JJJ:\\s*[^=\\n]+\\s*(,\\s*.+)*\\\)"
           :tests ("fn abc(test: u32) -> u32 {"
                   "fn abc(x: u32, y: u32, test: Vec<u32>, z: Vec<Foo>)"
                   "fn abc(x: u32, y: u32, test: &mut Vec<u32>, z: Vec<Foo>)"))

    ;; "if let" and "while let" desugaring
    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
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
    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "struct\\s+[^\\n{]+[{][^}]*(\\s*JJJ\\s*:\\s*[^\\n},]+)[^}]*}"
           :tests ("struct Foo { abc: u32, test: Vec<String>, b: PathBuf }"
                   "struct Foo<T>{test:Vec<T>}"
                   "struct FooBar<'a> { test: Vec<String> }")
           :not ("struct Foo { abc: u32, b: Vec<String> }"
                 "/// ... construct the equivalent ...\nfn abc() {\n"))

    ;; enum variants
    (:language "rust" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "enum\\s+[^\\n{]+\\s*[{][^}]*\\bJJJ\\b[^}]*}"
           :tests ("enum Foo { VariantA, test, VariantB(u32) }"
                   "enum Foo<T> { test(T) }"
                   "enum BadStyle{test}"
                   "enum Foo32 { Bar, testing, test(u8) }")
           :not ("enum Foo { testing }"))

    (:language "rust" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfn\\s+JJJ\\s*\\\("
           :tests ("fn test(asdf: u32)"
                   "fn test()"
                   "pub fn test()"))

    (:language "rust" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bmacro_rules!\\s+JJJ"
           :tests ("macro_rules! test"))

    (:language "rust" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "struct\\s+JJJ\\s*[{\\\(]?"
           :tests ("struct test(u32, u32)"
                   "struct test;"
                   "struct test { abc: u32, def: Vec<String> }"))

    (:language "rust" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "trait\\s+JJJ\\s*[{]?"
           :tests ("trait test;" "trait test { fn abc() -> u32; }"))

    (:language "rust" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\btype\\s+JJJ([^=\\n]+)?\\s*=[^=\\n]+;"
           :tests ("type test<T> = Rc<RefCell<T>>;"
                   "type test = Arc<RwLock<Vec<u32>>>;"))

    (:language "rust" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "impl\\s+((\\w+::)*\\w+\\s+for\\s+)?(\\w+::)*JJJ\\s+[{]?"
           :tests ("impl test {"
                   "impl abc::test {"
                   "impl std::io::Read for test {"
                   "impl std::io::Read for abc::test {"))

    (:language "rust" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "mod\\s+JJJ\\s*[{]?"
           :tests ("mod test;" "pub mod test {"))

    ;; enum definitions
    (:language "rust" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\benum\\s+JJJ\\b\\s*[{]?"
           :tests ("enum test { A, B, C }"
                   "enum test {"
                   "pub enum test<T> {"
                   "pub enum test<T> { A, B }"
                   "pub enum test<T: Clone + Debug> {"
                   "enum test { Variant1, Variant2(Type) }"
                   "pub(crate) enum test { Variant }")
           :not ("enum testing { A, B }"
                 "enum Foo { test, Bar }"))

    ;;-- elixir
    (:language "elixir" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bdef(p)?\\s+JJJ\\s*[ ,\\\(]"
           :tests ("def test do"
                   "def test, do:"
                   "def test() do"
                   "def test(), do:"
                   "def test(foo, bar) do"
                   "def test(foo, bar), do:"
                   "defp test do"
                   "defp test(), do:"))

    (:language "elixir" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*JJJ\\s*=[^=\\n]+"
           :tests ("test = 1234")
           :not ("if test == 1234"))

    (:language "elixir" :type "module"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "defmodule\\s+(\\w+\\.)*JJJ\\s+"
           :tests ("defmodule test do"
                   "defmodule Foo.Bar.test do"))

    (:language "elixir" :type "module"
           :supports ("ag" "grep" "rg" "git-grep") :
           :regex "defprotocol\\s+(\\w+\\.)*JJJ\\s+"
           :tests ("defprotocol test do"
                   "defprotocol Foo.Bar.test do"))

    ;;-- erlang
    (:language "erlang" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^JJJ\\b\\s*\\\("
           :tests ("test() ->"
                   "test()->"
                   "test(Foo) ->"
                   "test (Foo,Bar) ->"
                   "test(Foo, Bar)->"))

    (:language "erlang" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*JJJ\\s*=[^:=\\n]+"
           :tests ("test = 1234")
           :not ("if test =:= 1234"
                 "if test == 1234"))

    (:language "erlang" :type "module"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "^-module\\\(JJJ\\\)"
           :tests ("-module(test)."))

    ;;-- scss
    (:language "scss" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "@mixin\\sJJJ\\b\\s*\\\("
           :tests ("@mixin test()"))

    (:language "scss" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "@function\\sJJJ\\b\\s*\\\("
           :tests ("@function test()"))

    (:language "scss" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "JJJ\\s*:\\s*"
           :tests ("test  :"))

    ;;-- sml
    (:language "sml" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*(data)?type\\s+.*\\bJJJ\\b"
           :tests ("datatype test ="
                   "datatype test="
                   "datatype 'a test ="
                   "type test ="
                   "type 'a test ="
                   "type 'a test"
                   "type test")
           :not ("datatypetest ="))

    (:language "sml" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*val\\s+\\bJJJ\\b"
           :tests ("val test ="
                   "val test="
                   "val test : bool"))

    (:language "sml" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*fun\\s+\\bJJJ\\b.*\\s*="
           :tests ("fun test list ="
                   "fun test (STRING_NIL, a) ="
                   "fun test ((s1,s2): 'a queue) : 'a * 'a queue ="
                   "fun test (var : q) : int ="
                   "fun test f e xs ="))

    (:language "sml" :type "module"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*(structure|signature|functor)\\s+\\bJJJ\\b"
           :tests ("structure test ="
                   "structure test : MYTEST ="
                   "signature test ="
                   "functor test (T:TEST) ="
                   "functor test(T:TEST) ="))

    ;;-- sql
    (:language "sql" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(CREATE|create)\\s+(.+?\\s+)?(FUNCTION|function|PROCEDURE|procedure)\\s+JJJ\\s*\\\("
           :tests ("CREATE FUNCTION test(i INT) RETURNS INT"
                   "create or replace function test (int)"
                   "CREATE PROCEDURE test (OUT p INT)"
                   "create definer = 'test'@'localhost' procedure test()"))

    (:language "sql" :type "table"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(CREATE|create)\\s+(.+?\\s+)?(TABLE|table)(\\s+(IF NOT EXISTS|if not exists))?\\s+JJJ\\b"
           :tests ("CREATE TABLE test ("
                   "create temporary table if not exists test"
                   "CREATE TABLE IF NOT EXISTS test ("
                   "create global temporary table test"))

    (:language "sql" :type "view"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(CREATE|create)\\s+(.+?\\s+)?(VIEW|view)\\s+JJJ\\b"
           :tests ("CREATE VIEW test ("
                   "create sql security definer view test"
                   "CREATE OR REPLACE VIEW test AS foo"))

    (:language "sql" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(CREATE|create)\\s+(.+?\\s+)?(TYPE|type)\\s+JJJ\\b"
           :tests ("CREATE TYPE test"
                   "CREATE OR REPLACE TYPE test AS foo ("
                   "create type test as ("))

    ;;-- systemverilog
    (:language "systemverilog" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*class\\s+\\bJJJ\\b"
           :tests ("virtual class test;"
                   "class test;"
                   "class test extends some_class")
           :not ("virtual class testing;"
                 "class test2;"
                 "class some_test"
                 "class some_class extends test"))

    (:language "systemverilog" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*task\\s+\\bJJJ\\b"
           :tests ("task test ("
                   "task test(")
           :not ("task testing ("
                 "task test2("))

    (:language "systemverilog" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\b\\s*="
           :tests ("assign test ="
                   "assign test="
                   "int test ="
                   "int test=")
           :not ("assign testing ="
                 "assign test2="))

    (:language "systemverilog" :type "function" :
           supports ("ag" "rg" "git-grep")
           :regex "function\\s[^\\s]+\\s*\\bJJJ\\b"
           :tests ("function Matrix test ;"
                   "function Matrix test;")
           :not ("function test blah"))

    ;; matches SV class handle declarations
    (:language "systemverilog" :type "function"
           :supports ("ag" "rg" "git-grep")
           :regex "^\\s*[^\\s]*\\s*[^\\s]+\\s+\\bJJJ\\b"
           :tests ("some_class_name test"
                   "  another_class_name  test ;"
                   "some_class test[];"
                   "some_class #(1) test")
           :not ("test some_class_name"
                 "class some_class extends test"))

    ;;-- vhdl
    (:language "vhdl" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*type\\s+\\bJJJ\\b"
           :tests ("type test is"
                   "type test  is")
           :not ("type testing is"
                 "type test2  is"))

    (:language "vhdl" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*constant\\s+\\bJJJ\\b"
           :tests ("constant test :"
                   "constant test:")
           :not ("constant testing "
                 "constant test2:"))

    (:language "vhdl" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "function\\s*\"?JJJ\"?\\s*\\\("
           :tests ("function test(signal)"
                   "function test (signal)"
                   "function \"test\" (signal)")
           :not ("function testing(signal"))

    ;;-- latex
    (:language "tex" :type "command"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\\.*newcommand\\\*?\\s*\\\{\\s*(\\\\)JJJ\\s*}"
           :tests ("\\newcommand{\\test}"
                   "\\renewcommand{\\test}"
                   "\\renewcommand*{\\test}"
                   "\\newcommand*{\\test}"
                   "\\renewcommand{ \\test }")
           :not("\\test"  "test"))

    (:language "tex" :type "command"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\\.*newcommand\\\*?\\s*(\\\\)JJJ\\j"
           :tests ("\\newcommand\\test {}"
                   "\\renewcommand\\test{}"
                   "\\newcommand \\test")
           :not("\\test"
                "test"))

    (:language "tex" :type "length" :
           supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\\(s)etlength\\s*\\\{\\s*(\\\\)JJJ\\s*}"
           :tests ("\\setlength { \\test}"
                   "\\setlength{\\test}"
                   "\\setlength{\\test}{morecommands}" )
           :not("\\test"
                "test"))

    (:language "tex" :type "counter"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\\newcounter\\\{\\s*JJJ\\s*}"
           :tests ("\\newcounter{test}" )
           :not("\\test"
                "test"))

    (:language "tex" :type "environment"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\\\.*newenvironment\\s*\\\{\\s*JJJ\\s*}"
           :tests ("\\newenvironment{test}"
                   "\\newenvironment {test}{morecommands}"
                   "\\lstnewenvironment{test}"
                   "\\newenvironment {test}" )
           :not("\\test"
                "test" ))

    ;;-- pascal (todo: var, type, const)
    (:language "pascal" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bfunction\\s+JJJ\\b"
           :tests ("  function test : "))

    (:language "pascal" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bprocedure\\s+JJJ\\b"
           :tests ("  procedure test ; "))

    ;;-- f#
    (:language "fsharp" :type "variable"
           :supports ("ag" "grep" "git-grep")
           :regex "let\\s+JJJ\\b.*\\\="
           :tests ("let test = 1234"
                   "let test() = 1234"
                   "let test abc def = 1234")
           :not ("let testnot = 1234"
                 "let testnot() = 1234"
                 "let testnot abc def = 1234"))

    (:language "fsharp" :type "interface"
           :supports ("ag" "grep" "git-grep")
           :regex "member(\\b.+\\.|\\s+)JJJ\\b.*\\\="
           :tests ("member test = 1234"
                   "member this.test = 1234")
           :not ("member testnot = 1234"
                 "member this.testnot = 1234"))

    (:language "fsharp" :type "type"
           :supports ("ag" "grep" "git-grep")
           :regex "type\\s+JJJ\\b.*\\\="
           :tests ("type test = 1234")
           :not ("type testnot = 1234"))

    ;;-- kotlin
    (:language "kotlin" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "fun\\s*(<[^>]*>)?\\s*JJJ\\s*\\("
           :tests ("fun test()" "fun <T> test()"))

    (:language "kotlin" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(val|var)\\s*JJJ\\b"
           :not ("val testval"
                 "var testvar")
           :tests ("val test "
                   "var test"))

    (:language "kotlin" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test"
                   "class test : SomeInterface"
                   "interface test"))

    ;;-- zig
    (:language "zig" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "fn\\s+JJJ\\b"
           :tests ("fn test() void {"
                   "fn test(a: i32) i32 {"
                   "pub fn test(a: i32) i32 {"
                   "export fn test(a: i32) i32 {"
                   "extern \"c\" fn test(a: i32) i32 {"
                   "inline fn test(a: i32) i32 {"))

    (:language "zig" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(var|const)\\s+JJJ\\b"
           :tests ("const test: i32 = 3;"
                   "var test: i32 = 3;"
                   "pub const test: i32 = 3;"))

    ;;-- protobuf
    (:language "protobuf" :type "message"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "message\\s+JJJ\\s*\\\{"
           :tests ("message test{" "message test {"))

    (:language "protobuf" :type "enum"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "enum\\s+JJJ\\s*\\\{"
           :tests ("enum test{" "enum test {"))

    ;;-- apex (literally the same regexes as java)
    (:language "apex" :type "function"
           :supports ("ag" "rg")
           :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
           :tests ("int test()"
                   "int test(param)"
                   "static int test()"
                   "static int test(param)"
                   "public static MyType test()"
                   "private virtual SomeType test(param)"
                   "static int test()"
                   "private foo[] test()")
           :not ("test()" "testnot()"
                 "blah = new test()"
                 "foo bar = test()"))

    (:language "apex" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234")
           :not ("if test == 1234:" "int nottest = 44"))

    (:language "apex" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "(class|interface)\\s*JJJ\\b"
           :tests ("class test:"
                   "public class test implements Something")
           :not ("class testnot:"
                 "public class testnot implements Something"))

    ;;-- jai
    (:language "jai" :type "function"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*::"
           :tests ("test ::"))
    (:language "jai" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*(:|:\\s*=|::)"
           :tests ("test: Type" "test : Type = Val" "test :: Val"))
    (:language "jai" :type "type"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\bJJJ\\s*::"
           :tests ("test ::"))

    ;;-- odin
    (:language "odin" :type "variable"
           :supports ("ag" "grep" "rg" "git-grep")
           :regex "\\s*\\bJJJ\\s*:\\s*([^=\\n]+\\s*:|:|[^=\\n]+\\s*=|=)"
           :tests ("test :: struct"
                   "test ::enum"
                   "test:: union"
                   "test: : custom_type"
                   "test :: [2]f32"
                   "test : f32 : 20"
                   "test: i32 : 10"
                   "test := 20"
                   "test : f32 = 20"
                   "test: i32 = 10"
                   "test: i32= 10"
                   "test :i32= 10"
                   "test :: proc()"
                   "test ::proc() {"
                   "test:: proc(a: i32) -> i32 {"
                   "test::proc{}"
                   "test: :proc \"contextless\" {}"))

    ;;-- cobol
    (:language "cobol" :type "variable"
               :supports ("ag" "grep" "rg" "git-grep")
               :regex "^(.{6}[^*/])?\\s*([frs]d|[[:digit:]]{1,2})\\s+JJJ([\\. ]|$)"
               :tests ("       01 test."
                       "000350     10 test pic x."
                       "  88 test value '10'."
                       "fd  test."
                       "       sd test.")
               :not ("      *01 test"
                     "       05 test-01"
                     "       05 testing"))

    (:language "cobol" :type "section"
               :supports ("ag" "grep" "rg" "git-grep")
               :regex "^(.{6}[^*/])?JJJ(\\s+section)?\\."
               :tests ("       test."
                       "       test section."
                       "test."
                       "test section.")
               :not ("        test."
                     "      *test section."))

    (:language "cobol" :type "submodule"
               :supports ("ag" "grep" "rg" "git-grep")
               :regex "^(.{6}[^*/])?program-id\\.\\s*[\"']?JJJ[\"']?.*\\."
               :tests ("       program-id. test."
                       "       program-id. 'test'."
                       "program-id. \"test\".")
               :not ("        test.")))

  "List of search regex pattern templates organized by language and type.
Used for generating the grep tool search commands.
Notes:
- For a given language, a regular expression is only used by Dumb Jump when
  the currently used search tool is identified in the :supports value.

See relevant search tool command lines:

- Ag:
  https://github.com/ggreer/the_silver_searcher/blob/master/tests/list_file_types.t
- rg:
  https://github.com/BurntSushi/ripgrep/blob/master/crates/ignore/src/types.rs#L96

More information using the search tool command line help."
  :group 'dumb-jump
  :type
  '(repeat
    (plist :options ((:language string)
                     (:type string)
                     (:supports string)
                     (:regex string)
                     (:tests (repeat string))
                     (:not (repeat string))))))

(defcustom dumb-jump-language-file-exts
  ;;  dumb-jump language       file extension     ag type             rg type
  '((:language "elisp"         :ext "el"          :agtype "elisp"     :rgtype "elisp")
    (:language "elisp"         :ext "el.gz"       :agtype "elisp"     :rgtype "elisp")
    (:language "commonlisp"    :ext "lisp"        :agtype "lisp"      :rgtype "lisp")
    (:language "commonlisp"    :ext "lsp"         :agtype "lisp"      :rgtype "lisp")
    (:language "c++"           :ext "c"           :agtype "cc"        :rgtype "c")
    (:language "c++"           :ext "h"           :agtype "cc"        :rgtype "c")
    (:language "c++"           :ext "C"           :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "H"           :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "tpp"         :agtype "cpp"       :rgtype nil)
    (:language "c++"           :ext "cpp"         :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "hpp"         :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "cxx"         :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "hxx"         :agtype "cpp"       :rgtype nil)
    (:language "c++"           :ext "cc"          :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "hh"          :agtype "cpp"       :rgtype "cpp")
    (:language "c++"           :ext "c++"         :agtype nil         :rgtype nil)
    (:language "c++"           :ext "h++"         :agtype nil         :rgtype nil)
    (:language "coq"           :ext "v"           :agtype nil         :rgtype nil)
    (:language "ocaml"         :ext "ml"          :agtype "ocaml"     :rgtype "ocaml")
    (:language "ocaml"         :ext "mli"         :agtype "ocaml"     :rgtype "ocaml")
    (:language "ocaml"         :ext "mll"         :agtype "ocaml"     :rgtype "ocaml")
    (:language "ocaml"         :ext "mly"         :agtype "ocaml"     :rgtype "ocaml")
    ;; groovy is nil type because jenkinsfile is not in searcher type lists
    (:language "groovy"        :ext "gradle"      :agtype nil         :rgtype nil)
    (:language "groovy"        :ext "groovy"      :agtype nil         :rgtype nil)
    (:language "groovy"        :ext "jenkinsfile" :agtype nil         :rgtype nil)
    (:language "haskell"       :ext "hs"          :agtype "haskell"   :rgtype "haskell")
    (:language "haskell"       :ext "lhs"         :agtype "haskell"   :rgtype "haskell")
    (:language "objc"          :ext "m"           :agtype "objc"      :rgtype "objc")
    (:language "csharp"        :ext "cs"          :agtype "csharp"    :rgtype "csharp")
    (:language "java"          :ext "java"        :agtype "java"      :rgtype "java")
    (:language "vala"          :ext "vala"        :agtype "vala"      :rgtype "vala")
    (:language "vala"          :ext "vapi"        :agtype "vala"      :rgtype "vala")
    (:language "julia"         :ext "jl"          :agtype "julia"     :rgtype "julia")
    (:language "clojure"       :ext "clj"         :agtype "clojure"   :rgtype "clojure")
    (:language "clojure"       :ext "cljc"        :agtype "clojure"   :rgtype "clojure")
    (:language "clojure"       :ext "cljs"        :agtype "clojure"   :rgtype "clojure")
    (:language "clojure"       :ext "cljx"        :agtype "clojure"   :rgtype "clojure")
    (:language "coffeescript"  :ext "coffee"      :agtype "coffee"    :rgtype "coffeescript")
    (:language "faust"         :ext "dsp"         :agtype nil         :rgtype nil)
    (:language "faust"         :ext "lib"         :agtype nil         :rgtype nil)
    (:language "fennel"        :ext "fnl"         :agtype nil         :rgtype nil)
    (:language "fortran"       :ext "F"           :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "f"           :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "f77"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "f90"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "f95"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "F77"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "F90"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "F95"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "f03"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "for"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "ftn"         :agtype "fortran"   :rgtype "fortran")
    (:language "fortran"       :ext "fpp"         :agtype "fortran"   :rgtype "fortran")
    (:language "go"            :ext "go"          :agtype "go"        :rgtype "go")
    (:language "javascript"    :ext "js"          :agtype "js"        :rgtype "js")
    (:language "javascript"    :ext "jsx"         :agtype "js"        :rgtype "js")
    (:language "javascript"    :ext "vue"         :agtype "js"        :rgtype "js")
    (:language "javascript"    :ext "html"        :agtype "html"      :rgtype "html")
    (:language "javascript"    :ext "css"         :agtype "css"       :rgtype "css")
    (:language "typescript"    :ext "ts"          :agtype "ts"        :rgtype "ts")
    (:language "typescript"    :ext "tsx"         :agtype "ts"        :rgtype "ts")
    (:language "typescript"    :ext "vue"         :agtype "ts"        :rgtype "ts")
    (:language "dart"          :ext "dart"        :agtype nil         :rgtype "dart")
    (:language "lua"           :ext "lua"         :agtype "lua"       :rgtype "lua")
    ;; the extension "m" is also used by obj-c so must use matlab-mode
    ;; since obj-c will win by file extension, but here for searcher types
    (:language "matlab"        :ext "m"           :agtype "matlab"    :rgtype "matlab")
    (:language "nim"           :ext "nim"         :agtype "nim"       :rgtype "nim")
    (:language "nix"           :ext "nix"         :agtype "nix"       :rgtype "nix")
    (:language "org"           :ext "org"         :agtype nil         :rgtype "org")
    (:language "perl"          :ext "pl"          :agtype "perl"      :rgtype "perl")
    (:language "perl"          :ext "pm"          :agtype "perl"      :rgtype "perl")
    (:language "perl"          :ext "pm6"         :agtype "perl"      :rgtype nil)
    (:language "perl"          :ext "perl"        :agtype nil         :rgtype "perl")
    (:language "perl"          :ext "plh"         :agtype nil         :rgtype "perl")
    (:language "perl"          :ext "plx"         :agtype nil         :rgtype "perl")
    (:language "perl"          :ext "pod"         :agtype "perl"      :rgtype "pod")
    (:language "perl"          :ext "t"           :agtype "perl"      :rgtype nil)
    (:language "php"           :ext "php"         :agtype "php"       :rgtype "php")
    (:language "php"           :ext "php3"        :agtype "php"       :rgtype "php")
    (:language "php"           :ext "php4"        :agtype "php"       :rgtype "php")
    (:language "php"           :ext "php5"        :agtype "php"       :rgtype "php")
    (:language "php"           :ext "phtml"       :agtype "php"       :rgtype "php")
    (:language "php"           :ext "inc"         :agtype "php"       :rgtype nil)
    (:language "python"        :ext "py"          :agtype "python"    :rgtype "py")
    (:language "r"             :ext "R"           :agtype "r"         :rgtype "r")
    (:language "r"             :ext "r"           :agtype "r"         :rgtype "r")
    (:language "r"             :ext "Rmd"         :agtype "r"         :rgtype "r")
    (:language "r"             :ext "Rnw"         :agtype "r"         :rgtype "r")
    (:language "r"             :ext "Rtex"        :agtype "r"         :rgtype nil)
    (:language "r"             :ext "Rrst"        :agtype "r"         :rgtype nil)
    (:language "racket"        :ext "rkt"         :agtype "racket"    :rgtype "lisp")
    (:language "crystal"       :ext "cr"          :agtype "crystal"   :rgtype "crystal")
    (:language "crystal"       :ext "ecr"         :agtype "crystal"   :rgtype nil)
    (:language "ruby"          :ext "rb"          :agtype "ruby"      :rgtype "ruby")
    (:language "ruby"          :ext "erb"         :agtype "ruby"      :rgtype nil)
    (:language "ruby"          :ext "haml"        :agtype "ruby"      :rgtype nil)
    (:language "ruby"          :ext "rake"        :agtype "ruby"      :rgtype nil)
    (:language "ruby"          :ext "slim"        :agtype "ruby"      :rgtype nil)
    (:language "rust"          :ext "rs"          :agtype "rust"      :rgtype "rust")
    (:language "zig"           :ext "zig"         :agtype nil         :rgtype "zig")
    (:language "scad"          :ext "scad"        :agtype nil         :rgtype nil)
    (:language "scala"         :ext "scala"       :agtype "scala"     :rgtype "scala")
    (:language "scheme"        :ext "scm"         :agtype "scheme"    :rgtype "lisp")
    (:language "scheme"        :ext "ss"          :agtype "scheme"    :rgtype "lisp")
    (:language "scheme"        :ext "sld"         :agtype "scheme"    :rgtype "lisp")
    (:language "janet"         :ext "janet"       :agtype "janet"     :rgtype "lisp")
    (:language "shell"         :ext "sh"          :agtype nil         :rgtype nil)
    (:language "shell"         :ext "bash"        :agtype nil         :rgtype nil)
    (:language "shell"         :ext "csh"         :agtype nil         :rgtype nil)
    (:language "shell"         :ext "ksh"         :agtype nil         :rgtype nil)
    (:language "shell"         :ext "tcsh"        :agtype nil         :rgtype nil)
    (:language "sml"           :ext "sml"         :agtype "sml"       :rgtype "sml")
    (:language "solidity"      :ext "sol"         :agtype nil         :rgtype nil)
    (:language "sql"           :ext "sql"         :agtype "sql"       :rgtype "sql")
    (:language "swift"         :ext "swift"       :agtype nil         :rgtype "swift")
    (:language "tex"           :ext "tex"         :agtype "tex"       :rgtype "tex")
    (:language "elixir"        :ext "ex"          :agtype "elixir"    :rgtype "elixir")
    (:language "elixir"        :ext "exs"         :agtype "elixir"    :rgtype "elixir")
    (:language "elixir"        :ext "eex"         :agtype "elixir"    :rgtype "elixir")
    (:language "erlang"        :ext "erl"         :agtype "erlang"    :rgtype "erlang")
    (:language "systemverilog" :ext "sv"          :agtype "verilog"   :rgtype "verilog")
    (:language "systemverilog" :ext "svh"         :agtype "verilog"   :rgtype "verilog")
    (:language "vhdl"          :ext "vhd"         :agtype "vhdl"      :rgtype "vhdl")
    (:language "vhdl"          :ext "vhdl"        :agtype "vhdl"      :rgtype "vhdl")
    (:language "scss"          :ext "scss"        :agtype "css"       :rgtype "css")
    (:language "pascal"        :ext "pas"         :agtype "delphi"    :rgtype nil)
    (:language "pascal"        :ext "dpr"         :agtype "delphi"    :rgtype nil)
    (:language "pascal"        :ext "int"         :agtype "delphi"    :rgtype nil)
    (:language "pascal"        :ext "dfm"         :agtype "delphi"    :rgtype nil)
    (:language "fsharp"        :ext "fs"          :agtype "fsharp"    :rgtype nil)
    (:language "fsharp"        :ext "fsi"         :agtype "fsharp"    :rgtype nil)
    (:language "fsharp"        :ext "fsx"         :agtype "fsharp"    :rgtype nil)
    (:language "kotlin"        :ext "kt"          :agtype "kotlin"    :rgtype "kotlin")
    (:language "kotlin"        :ext "kts"         :agtype "kotlin"    :rgtype "kotlin")
    (:language "protobuf"      :ext "proto"       :agtype "proto"     :rgtype "protobuf")
    (:language "hcl"           :ext "tf"          :agtype "terraform" :rgtype "tf")
    (:language "hcl"           :ext "tfvars"      :agtype "terraform" :rgtype nil)
    (:language "apex"          :ext "cls"         :agtype nil         :rgtype nil)
    (:language "apex"          :ext "trigger"     :agtype nil         :rgtype nil)
    (:language "jai"           :ext "jai"         :agtype nil         :rgtype nil)
    (:language "odin"          :ext "odin"        :agtype nil         :rgtype nil)
    (:language "cobol"         :ext "cbl"         :agtype nil         :rgtype nil)
    (:language "cobol"         :ext "cob"         :agtype nil         :rgtype nil)
    (:language "cobol"         :ext "cpy"         :agtype nil         :rgtype nil))

  "Map programming language to file extensions support by search tool.
The value of :agytpe, :rgtype is nil when the search tool file type does
not search for that file extension, otherwise it is the name of the file
type for that tool."
  :group 'dumb-jump
  :type
  '(repeat
    (plist
     :options ((:language (string :tag "Language"))
               (:ext (string :tag "Extension"))
               (:agtype (string :tag "Ag type"))
               (:rgtype (string :tag "Ripgrep type"))))))

(defcustom dumb-jump-language-contexts
  '((:language "javascript" :type "function"  :left nil           :right "^(" )
    (:language "javascript" :type "variable"  :left "($"          :right nil )
    (:language "javascript" :type "variable"  :left "($"          :right "^)" )
    (:language "javascript" :type "variable"  :left nil           :right "^\\." )
    (:language "javascript" :type "variable"  :left nil           :right "^;" )
    (:language "typescript" :type "function"  :left nil           :right "^(" )
    (:language "perl"       :type "function"  :left nil           :right "^(" )
    (:language "tcl"        :type "function"  :left "\\[$"        :right nil)
    (:language "tcl"        :type "function"  :left "^\\s*$"      :right nil)
    (:language "tcl"        :type "variable"  :left "\\$$"        :right nil)
    (:language "php"        :type "function"  :left nil           :right "^(" )
    (:language "php"        :type "class"     :left "new\\s+"     :right nil )
    (:language "elisp"      :type "function"  :left "($"          :right nil )
    (:language "elisp"      :type "variable"  :left nil           :right "^)" )
    (:language "scheme"     :type "function"  :left "($"          :right nil )
    (:language "scheme"     :type "variable"  :left nil           :right "^)" )
    (:language "jai"        :type "function"  :left nil           :right "\\s*(" )
    (:language "jai"        :type "type"      :left "\\s*:\\s*"   :right nil)
    (:language "cobol"      :type "submodule" :left "call\\s+"    :right nil )
    (:language "cobol"      :type "section"   :left "perform\\s+" :right nil )
    (:language "cobol"      :type "section"   :left "go to\\s+"   :right nil ))
  "List of under points contexts for each language.
This helps limit the number of regular expressions we use
if we know that if there's a ( immediately to the right of
a symbol then it's probably a function call."
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
  '(".dumbjump"
    ".projectile"
    ".git"
    ".hg"
    ".fslckout"
    ".bzr"
    "_darcs"
    ".svn"
    "Makefile"
    "PkgInfo"
    "-pkg.el"
    "_FOSSIL_")
  "Files and directories that signify a directory is a project root.
If any of these denoting files are present in project sub-directories
(like Makefile) then you can either remove the culprit denoter from the
list or create a .dumbjumpignore file in the project sub-directory."
  :group 'dumb-jump
  :type '(repeat (string  :tag "Name")))

(defcustom dumb-jump-default-project "~"
  "Directory of default project to search within if a project root is not found."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-project nil
  "Directory of project to search within if normal denoters will not work.
This should only be needed in the rarest of cases."
  :group 'dumb-jump
  :type 'string)

(defcustom dumb-jump-before-jump-hook nil
  "Hooks called before jumping."
  :group 'dumb-jump
  :type 'hook)

(defcustom dumb-jump-after-jump-hook nil
  "Hooks called after jumping."
  :group 'dumb-jump
  :type 'hook)

(defcustom dumb-jump-aggressive
  nil
  "If `t` jump aggressively with the possibility of a false positive.
If `nil` always show list of more than 1 match."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-debug
  nil
  "If `t` will print helpful debug information.
Also ensure that `dumb-jump-quiet' is nil for debugging!"
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-confirm-jump-to-modified-file
  t
  "Confirm before jumping to a modified file?
If t, confirm before jumping to a modified file (which may lead to an
inaccurate jump).
If nil, jump without confirmation but print a warning."
  :group 'dumb-jump
  :type 'boolean)

(defcustom dumb-jump-disable-obsolete-warnings nil
  "If non-nil, don't warn about using the legacy interface."
  :group 'dumb-jump
  :type 'boolean)

;; ---------------------------------------------------------------------------
;; Debugging Utilities
;; -------------------

(defun dumb-jump-message-prin1 (format-str &rest args)
  "Helper debugging function: generate a FORMAT-STR message with all ARGS."
  ;; Fist transform each of ARGS to string with `prin1-to-string' then
  ;; pass them as arguments to message by making a list used the third
  ;; parameter to apply.
  (apply 'message format-str (-map 'prin1-to-string args)))

;; ---------------------------------------------------------------------------
;; Search tool presence checkers
;; -----------------------------
(defvar dumb-jump--detected-env-problems nil
  "List of detected environment problems.
Last detected problem is first in the list.")

(defun dumb-jump-env-problem (problem)
  "Add PROBLEM string to `dumb-jump--detected-env-problems' list."
  (unless (member problem dumb-jump--detected-env-problems)
    (push problem dumb-jump--detected-env-problems)))

;;;###autoload
(defun dumb-jump-show-detected-problems (keep)
  "Print all problems detected by dumb-jump environment in *Message*.
Erase the accumulated list unless the command is issued with any prefix
key (such as `\\[universal-argument]')."
  (interactive "P")
  (if dumb-jump--detected-env-problems
      (progn
        (message "-- Dumb-Jump Detected Environment Problems:")
        (dolist (problem (reverse dumb-jump--detected-env-problems))
          (message problem))
        (unless keep
          (setq dumb-jump--detected-env-problems nil)))
    (message "No problem recorded yet.")))

;; --

(defvar dumb-jump--ag-installed? 'unset)
(defun dumb-jump-ag-installed? ()
  "Return t if ag is installed."
  (if (eq dumb-jump--ag-installed? 'unset)
      (setq dumb-jump--ag-installed?
            (s-contains? "ag version"
                         (shell-command-to-string
                          (concat dumb-jump-ag-cmd " --version"))))
    dumb-jump--ag-installed?))

(defvar dumb-jump--git-grep-plus-ag-installed? 'unset)
(defun dumb-jump-git-grep-plus-ag-installed? ()
  "Return t if git grep and ag is installed."
  (if (eq dumb-jump--git-grep-plus-ag-installed? 'unset)
      (setq dumb-jump--git-grep-plus-ag-installed?
            (and (dumb-jump-git-grep-installed?)
                 (dumb-jump-ag-installed?)))
    dumb-jump--git-grep-plus-ag-installed?))

(defvar dumb-jump--rg-installed? 'unset)
(defun dumb-jump-rg-installed? ()
  "Return t if rg 0.10 or later with PCRE2 support is installed.
Return nil otherwise.  In that case store diagnostics information in the
variable `dumb-jump--detected-env-problems'."
  (if (eq dumb-jump--rg-installed? 'unset)
      (setq
       dumb-jump--rg-installed?
       (catch 'rg-problem
         (unless (executable-find dumb-jump-rg-cmd)
           (dumb-jump-env-problem
            (format "Ripgrep not found as specified in `dumb-jump-rg-cmd': %s"
                    dumb-jump-rg-cmd))
           (throw 'rg-problem nil))
         (let* ((stdout (shell-command-to-string
                         (concat dumb-jump-rg-cmd " --version")))
                (has-version (string-match "ripgrep \\([0-9]+\\.[0-9]+\\).*"
                                           stdout)))
           (unless has-version
             (dumb-jump-env-problem "Can't detect Ripgrep version.")
             (throw 'rg-problem nil))
           (unless (version<= "0.10" (match-string-no-properties 1 stdout))
             (dumb-jump-env-problem "Ripgrep >= 0.10 is not available.")
             (throw 'rg-problem nil))
           (unless (string-match "features:.*pcre2" stdout)
             (dumb-jump-env-problem "Ripgrep does not support PCRE2.")
             (throw 'rg-problem nil))
           t)))
    dumb-jump--rg-installed?))

(defvar dumb-jump--git-grep-installed? 'unset)
(defun dumb-jump-git-grep-installed? ()
  "Return t if git-grep is installed."
  (if (eq dumb-jump--git-grep-installed? 'unset)
      (setq dumb-jump--git-grep-installed?
            (s-contains? "fatal: no pattern given"
                         (shell-command-to-string
                          (concat dumb-jump-git-grep-cmd))))
    dumb-jump--git-grep-installed?))

(defvar dumb-jump--grep-installed? 'unset)
(defun dumb-jump-grep-installed? ()
  "Return non-nil if a grep is installed, nil otherwise.
Return \\='gnu if GNU grep is installed, \\='bsd if BSD grep is installed."
  (if (eq dumb-jump--grep-installed? 'unset)
      (let* ((version (shell-command-to-string
                       (concat dumb-jump-grep-cmd " --version")))
             (variant (cond ((s-match "GNU grep" version) 'gnu)
                            ((s-match "[0-9]+\\.[0-9]+" version) 'bsd)
                            (t nil))))
        (setq dumb-jump--grep-installed? variant))
    dumb-jump--grep-installed?))

;; ---------------------------------------------------------------------------
;; Dumb-Jump Tests
;; ---------------

(defun dumb-jump-run-test (test cmd)
  "Use TEST as the standard input for the CMD."
  (with-temp-buffer
    (insert test)
    (shell-command-on-region (point-min) (point-max) cmd nil t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dumb-jump-run-test-temp-file (test thefile realcmd)
  "Write TEST string to temporary file THEFILE, run shell REALCMD on it.
Return resulting string."
  (with-temp-buffer
    (insert test)
    (write-file thefile nil)
    (delete-region (point-min) (point-max))
    (shell-command realcmd t)
    (set-buffer-modified-p nil) ; prevent prompt on file deletion during test
    (delete-file thefile)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dumb-jump-run-git-grep-test (test git-grep-cmd)
  "Execute GIT-GREP-CMD on input TEST string written in temporary file.
Necessary because git grep must be given a file as input, not just a string."
  (let ((thefile ".git.grep.test"))
    (dumb-jump-run-test-temp-file test
                                  thefile
                                  (concat git-grep-cmd " " thefile))))

(defun dumb-jump-run-ag-test (test ag-cmd)
  "Execute AG-CMD on input TEST written in temporary file.
The difference is that ag ignores multiline matches when passed
input from stdin, which is a crucial feature."
  (let ((thefile ".ag.test"))
    (dumb-jump-run-test-temp-file test
                                  thefile
                                  (concat ag-cmd " " thefile))))

;; ---------------------------------------------------------------------------
;; Test dumb-jump search rules
;; ---------------------------

(defun dumb-jump--search-failure-msg (tool test run-not-tests resp cmd rule)
  "Return search failure string adjusted to the arguments.
TOOL: search tool name.
TEST: the test string.
RUN-NO-TESTS: t to see a list of all failed rules.
RESP: response.
CMD: the search command used.
RULE: the complete `dumb-jump-find-rules' rule used for this test."
  (format "%s FAILURE '%s' %s in response '%s' | CMD: '%s' | rule: '%s'"
          tool
          test
          (if run-not-tests "IS unexpectedly" "NOT")
          resp
          cmd
          rule))

(defun dumb-jump-test-grep-rules (&optional run-not-tests)
  "Test all the grep rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules."
  (let ((variant (if (eq (dumb-jump-grep-installed?) 'gnu) 'gnu-grep 'grep)))
    (-mapcat
     (lambda (rule)
       (-mapcat
        (lambda (test)
          (let* ((cmd (concat "grep -En -e "
                              (shell-quote-argument
                               (dumb-jump-populate-regex
                                (plist-get rule :regex) "test" variant))))
                 (resp (dumb-jump-run-test test cmd)))
            (when (or
                   (and (not run-not-tests) (not (s-contains? test resp)))
                   (and run-not-tests (> (length resp) 0)))
              (list (dumb-jump--search-failure-msg "grep"
                                                   test
                                                   run-not-tests
                                                   resp
                                                   cmd
                                                   (plist-get rule :regex))))))
        (plist-get rule (if run-not-tests :not :tests))))
     (--filter (member "grep" (plist-get it :supports)) dumb-jump-find-rules))))

(defun dumb-jump-test-ag-rules (&optional run-not-tests)
  "Test all the ag rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules."
  (-mapcat
   (lambda (rule)
     (-mapcat
      (lambda (test)
        (let* ((cmd (concat "ag --nocolor --nogroup --nonumber "
                            (shell-quote-argument
                             (dumb-jump-populate-regex
                              (plist-get rule :regex) "test" 'ag))))
               (resp (dumb-jump-run-ag-test test cmd)))
          (when (or
                 (and (not run-not-tests) (not (s-contains? test resp)))
                 (and run-not-tests (> (length resp) 0)))
            (list (dumb-jump--search-failure-msg "ag"
                                                 test
                                                 run-not-tests
                                                 resp
                                                 cmd
                                                 rule)))))
      (plist-get rule (if run-not-tests :not :tests))))
   (--filter (member "ag" (plist-get it :supports)) dumb-jump-find-rules)))

(defun dumb-jump-test-rg-rules (&optional run-not-tests)
  "Test all the rg rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules."
  (-mapcat
   (lambda (rule)
     (-mapcat
      (lambda (test)
        (let* ((cmd (concat "rg --color never --no-heading -U --pcre2 "
                            (shell-quote-argument
                             (dumb-jump-populate-regex
                              (plist-get rule :regex) "test" 'rg))))
               (resp (dumb-jump-run-test test cmd)))
          (when (or
                 (and (not run-not-tests) (not (s-contains? test resp)))
                 (and run-not-tests (> (length resp) 0)))
            (list (dumb-jump--search-failure-msg "rg"
                                                 test
                                                 run-not-tests
                                                 resp
                                                 cmd
                                                 rule)))))
      (plist-get rule (if run-not-tests :not :tests))))
   (--filter (member "rg" (plist-get it :supports)) dumb-jump-find-rules)))

(defun dumb-jump-test-git-grep-rules (&optional run-not-tests)
  "Test all the git grep rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules."
  (-mapcat
   (lambda (rule)
     (-mapcat
      (lambda (test)
        ;; -h: suppress output of filename on each match
        ;; --untracked : in addition to searching tracked file in working
        ;;               tree , search also in untracked files.
        ;; -E : POSIX extended/basic regexp to search.
        (let* ((cmd (concat "git grep --color=never -h --untracked -E  "
                            (shell-quote-argument
                             (dumb-jump-populate-regex
                              (plist-get rule :regex) "test" 'git-grep))))
               (resp (dumb-jump-run-git-grep-test test cmd)))
          (when (or
                 (and (not run-not-tests) (not (s-contains? test resp)))
                 (and run-not-tests (> (length resp) 0)))
            (list (dumb-jump--search-failure-msg "git-grep"
                                                 test
                                                 run-not-tests
                                                 resp
                                                 cmd
                                                 rule)))))
      (plist-get rule (if run-not-tests :not :tests))))
   (--filter (member "grep" (plist-get it :supports)) dumb-jump-find-rules)))

;; ---------------------------------------------------------------------------

(defun dumb-jump-message (str &rest args)
  "Log message STR with ARGS to *Messages* if not using `dumb-jump-quiet'."
  (unless dumb-jump-quiet
    (apply 'message str args))
  nil)

(defmacro dumb-jump-debug-message (&rest exprs)
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
        (insert "DUMB JUMP DEBUG `")
        (insert defun-name)
        (insert "` START\n----\n\n")
        (dolist (expr exprs)
          (insert (prin1-to-string expr) ":\n\t%s\n\n"))
        (insert "\n-----\nDUMB JUMP DEBUG `")
        (insert defun-name)
        (insert "` END\n-----")
        `(when dumb-jump-debug
           (dumb-jump-message
            ,(buffer-string)
            ,@exprs))))))

(defun dumb-jump-get-point-context (line func cur-pos)
  "Get the LINE context to the left and right of FUNC using CUR-POS as hint."
  (let ((loc (or (cl-search func line :start2 cur-pos) 0)))
    (list :left (substring line 0 loc)
          :right (substring line (+ loc (length func))))))

(defun dumb-jump-to-selected (results choices selected)
  "With RESULTS use CHOICES to find the SELECTED choice from multiple options."
  (let* ((result-index (--find-index (string= selected it) choices))
         (result (when result-index
                   (nth result-index results))))
    (when result
      (dumb-jump-result-follow result))))

(defun dumb-jump-helm-persist-action (candidate)
  "Preview CANDIDATE in temporary buffer displaying the file at matched line.
\\<helm-map>
This is the persistent action (\\[helm-execute-persistent-action]) for helm."
  (let* ((file (plist-get candidate :path))
         (line (plist-get candidate :line))
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

(defun dumb-jump--format-result (proj result)
  "Return formatted string for PROJ and RESULT."
  (format "%s:%s: %s"
          (s-replace proj "" (plist-get result :path))
          (plist-get result :line)
          (s-trim (plist-get result :context))))

(defun dumb-jump-ivy-jump-to-selected (results choices _proj)
  "Offer CHOICES as candidates through `ivy-read'.
Then execute dumb-jump-result-follow' on the selected choice RESULTS.
Ignore _PROJ."
  (if (fboundp 'ivy-read)
      (ivy-read "Jump to: " (-zip-pair choices results)
                :action (lambda (cand)
                          (dumb-jump-result-follow (cdr cand)))
                :caller 'dumb-jump-ivy-jump-to-selected)
    (error "ivy-read is unknown.  Is it loaded?")))

(defun dumb-jump-prompt-user-for-choice (proj results)
  "Put a PROJ list of RESULTS in menu for user selection.
The menu can be a `popup-menu' or helm/ivy menu.
Filters PROJ path from files for display."
  (let ((choices (--map (dumb-jump--format-result proj it) results)))
    (cond
     ((eq dumb-jump-selector 'completing-read)
      (dumb-jump-to-selected results choices
                             (completing-read "Jump to: " choices)))
     ((and (eq dumb-jump-selector 'ivy)
           (fboundp 'ivy-read))
      (funcall dumb-jump-ivy-jump-to-selected-function results choices proj))
     ((and (eq dumb-jump-selector 'helm)
           (fboundp 'helm))
      (helm :sources
            (with-no-warnings
              (helm-make-source "Jump to: " 'helm-source-sync
                :action '(("Jump to match" . dumb-jump-result-follow))
                :candidates (-zip-pair choices results)
                :persistent-action 'dumb-jump-helm-persist-action))
            :buffer "*helm dumb jump choices*"))
     (t ; or popup
      (dumb-jump-to-selected results choices (popup-menu* choices))))))

(defun dumb-jump-get-project-root (filepath)
  "Return project root holding FILEPATH.
Keep looking at the parent dir of FILEPATH until we find a denoter file/dir.
Return a directory name without the trailing slash."
  (directory-file-name
   (expand-file-name
    (or
     dumb-jump-project
     (locate-dominating-file filepath #'dumb-jump-get-config)
     dumb-jump-default-project))))

(defun dumb-jump-get-config (dir)
  "If a project denoter is in DIR then return it, otherwise nil.
However, if DIR contains a `.dumbjumpignore' it returns nil to keep
looking for another root."
  (if (file-exists-p (expand-file-name ".dumbjumpignore" dir))
      nil
    (car (--filter
          (file-exists-p (expand-file-name it dir))
          dumb-jump-project-denoters))))

(defun dumb-jump-get-language (file)
  "Get language from FILE extension.  Fallback to using `major-mode' name."
  (let* ((language (or (dumb-jump-get-language-from-mode)
                       (dumb-jump-get-language-by-filename file)
                       (dumb-jump-get-mode-base-name))))
                                        ; src edit buffer ? org-edit-src-exit
    (if (and (fboundp 'org-src-edit-buffer-p)
             (org-src-edit-buffer-p))
        (progn
          (setq language "org")
          (with-no-warnings
            (org-edit-src-exit))
          (if (and (boundp 'org-version)
                   (version< org-version "9"))
            (save-buffer))))
    (if (string= language "org")
        (setq language (dumb-jump-get-language-in-org)))
    (if (member language (-distinct
                          (--map (plist-get it :language)
                                 dumb-jump-find-rules)))
        language
      (format ".%s file" (or (file-name-extension file) "")))))

(defun dumb-jump-get-language-in-org ()
  "Return associated language in org mode src block or \"org\" if outside src.
This assumes point is inside an org mode buffer."
  (let ((lang (nth 0 (with-no-warnings (org-babel-get-src-block-info)))))
    ;; if lang exists then create a composite language
    (if lang
        (dumb-jump-make-composite-language
         "org"
         (if (dumb-jump-get-language-from-aliases lang)
             (dumb-jump-get-language-from-aliases lang)
           lang)
         "org"
         "org"
         "org")
      "org")))

(defun dumb-jump-add-language-to-proplist (newlang proplist lang)
  "Return nil if NEWLANG  is already in PROPLIST or (if not)
return a new proplist.  The new proplis is PROPLIS
where a NEWLANG plist(s) is (are) added to PROPLIST.
The plist(s) value of NEWLANG is (are) copied from
those of LANG and LANG is replaced by NEWLANG."
  (unless (--filter (string= newlang (plist-get it :language))
                   proplist)
      (--splice
       (string= lang (plist-get it :language))
       (list it (plist-put (copy-tree it) :language newlang))
       proplist)))

(defun dumb-jump-make-composite-language (mode lang extension agtype rgtype)
  "Concat one MODE  (usually the string org) with a LANG  (c or python or etc)
to make a composite language of the form cPLUSorg or pythonPLUSorg or etc.
Modify `dumb-jump-find-rules' and `dumb-jump-language-file-exts' accordingly
\(using EXTENSION AGTYPE RGTYPE)"
  (let* ((complang (concat lang "PLUS" mode))
         (alreadyextension (--filter (and
                                      (string= complang (plist-get it :language))
                                      (string= extension (plist-get it :ext))
                                      (string= agtype (plist-get it :agtype))
                                      (string= rgtype (plist-get it :rgtype)))
                                     dumb-jump-language-file-exts))
         (newfindrule (dumb-jump-add-language-to-proplist complang dumb-jump-find-rules lang))
         (newfileexts (dumb-jump-add-language-to-proplist complang dumb-jump-language-file-exts lang)))
    ;; add (if needed) composite language to dumb-jump-find-rules
    (when newfindrule
      (set-default 'dumb-jump-find-rules newfindrule))
    ;; add (if needed) composite language to dumb-jump-language-file-exts
    (unless dumb-jump-search-type-org-only-org
      (when newfileexts
        (set-default 'dumb-jump-language-file-exts newfileexts)))
    ;; add (if needed) a new extension to dumb-jump-language-file-exts
    (unless alreadyextension
        (set-default 'dumb-jump-language-file-exts
                     (cons `(:language ,complang
                             :ext ,extension
                             :agtype ,agtype
                             :rgtype ,rgtype)
                           dumb-jump-language-file-exts)))
    complang))

(defcustom dumb-jump-language-aliases-alist
  '(("sh" . "shell")
    ("cperl" . "perl")
    ("octave" . "matlab")
    ("emacs-lisp" . "elisp")
    ("R" . "r"))
  "An alist of language alias to language name.
Aliases are major mode base name sometimes used for some files.
For example: \"cperl\" for the \"perl\" language."
  :group 'dumb-jump
  :type '(repeat (cons
                  (string :tag "alias   ")
                  (string :tag "language"))))

(defun dumb-jump-get-language-from-aliases (lang)
  "Return the dumb-jump language name for a possible alias if there is one.
Return nil if LANG is not one of the supported aliases."
  (assoc-default lang dumb-jump-language-aliases-alist))

(defun dumb-jump-get-mode-base-name ()
  "Get the base name of the mode."
  (s-replace "-mode" "" (symbol-name major-mode)))

(defun dumb-jump-get-language-from-mode ()
  "Extract the language from the `major-mode' name.
Currently just everything before \"-mode\"."
  (dumb-jump-get-language-from-aliases (dumb-jump-get-mode-base-name)))

(defun dumb-jump-get-language-by-filename (file)
  "Get the programming language name from the specified FILE extension."
  ;; Search in `dumb-jump-language-file-exts' for an entry that has the :ext
  ;; attribute equal to the file extension and return the corresponding
  ;; :language value.
  (let* ((filename (if (string-suffix-p ".gz" file)
                       (file-name-sans-extension file)
                     file))
         (result (--filter
                  (string-suffix-p (concat "." (plist-get it :ext)) filename)
                  dumb-jump-language-file-exts)))
    (when (and result (eq (length result) 1))
      (plist-get (car result) :language))))

;; --

(defun dumb-jump-issue-result (issue)
  "Return a result property list with the ISSUE set as :issue property symbol."
  `(:results nil
    :lang nil
    :symbol nil
    :ctx-type nil
    :file nil
    :root nil
    :issue ,(intern issue)))

(defun dumb-jump-get-results (&optional prompt)
  "Perform the dumb-jump search for text at point, region or in PROMPT string.

Runs `dumb-jump-fetch-results' under conditions adjusted to the current
context.

Perform the search if searcher is installed, buffer is saved, and
there's a symbol under point or an item to search is specified by the
PROMPT argument.

Return the dumb-jump alist result:
- :results
- :lang    : string: language name
- :symbol  : string: symbol being searched
- :ctx-type: string:
- :file    : string: file name
- :root    : string: project root
- :issue   : symbol: problem description symbol: nogrep or nosymbol."
  (cond
   ;; When no search tool is available just return an empty result identifying
   ;; a nogrep issue.
   ((not (or (dumb-jump-ag-installed?)
             (dumb-jump-rg-installed?)
             (dumb-jump-git-grep-installed?)
             (dumb-jump-grep-installed?)))
    (dumb-jump-issue-result "nogrep"))
   ;; Inside a shell or eshell buffer,
   ((or (string= (buffer-name) "*shell*")
        (string= (buffer-name) "*eshell*"))
    (dumb-jump-fetch-shell-results prompt))
   ;; When inside a normal buffer but no string specified by PROMPT,
   ;; point or region does not identify something to search,
   ;; just return an empty result identifying a nosymbol issue.
   ((and (not prompt)
         (not (region-active-p))
         (not (thing-at-point 'symbol)))
    (dumb-jump-issue-result "nosymbol"))
   ;; Otherwise, perform the search for the text identified by string specified
   ;; by PROMPT, point or region.
   (t
    (dumb-jump-fetch-file-results prompt))))

(defun dumb-jump-fetch-shell-results (&optional prompt)
  "Perform search from shell buffer for text at point, region or PROMPT string.
Return the dumb-jump alist result:
- :results
- :lang    : string: language name
- :symbol  : string: symbol being searched
- :ctx-type: string:
- :file    : string: file name
- :root    : string: project root."
  (let* ((cur-file (buffer-name))
         (proj-root (dumb-jump-get-project-root default-directory))
         (proj-config (dumb-jump-get-config proj-root))
         (config (when (string-suffix-p ".dumbjump" proj-config)
                   (dumb-jump-read-config proj-root proj-config)))
         (lang (or (plist-get config :language)
                   (car (dumb-jump-get-lang-by-shell-contents (buffer-name))))))
    (dumb-jump-fetch-results cur-file proj-root lang config prompt)))

(defun dumb-jump-fetch-file-results (&optional prompt)
  "Perform a file search for text at point, region or PROMPT string.
Return the dumb-jump alist result:
- :results
- :lang    : string: language name
- :symbol  : string: symbol being searched
- :ctx-type: string:
- :file    : string: file name
- :root    : string: project root."
  (let* ((cur-file (or (buffer-file-name) ""))
         (proj-root (dumb-jump-get-project-root cur-file))
         (proj-config (dumb-jump-get-config proj-root))
         (config (when (string-suffix-p ".dumbjump" proj-config)
                   (dumb-jump-read-config proj-root proj-config)))
         (lang (or (plist-get config :language)
                   (dumb-jump-get-language cur-file))))
    (dumb-jump-fetch-results cur-file proj-root lang config prompt)))

;; --
(defun dumb-jump-process-symbol-by-lang (lang look-for)
  "Process and potentially transform LANG's LOOK-FOR.
For instance, remove clojure namespace prefix."
  (cond
   ((and (string= lang "commonlisp")
         (s-contains? ":" look-for)
         (not (string-prefix-p ":" look-for)))
    (nth 1 (s-split ":" look-for 'omit-nulls)))
   ((and (string= lang "clojure")
         (s-contains? "/" look-for))
    (nth 1 (s-split "/" look-for)))
   ((and (string= lang "fennel")
         (s-contains? "." look-for))
    (-last-item (s-split "\\." look-for)))
   ((and (string= lang "ruby")
         (s-contains? "::" look-for))
    (-last-item (s-split "::" look-for)))
   ((and (or (string= lang "ruby")
             (string= lang "crystal"))
         (string-prefix-p ":" look-for))
    (s-chop-prefix ":" look-for))
   ((and (string= lang "systemverilog")
         (string-prefix-p "`" look-for))
    (s-chop-prefix "`" look-for))
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

(defun dumb-jump--get-symbol-start ()
  "Get the start of symbol at point."
  (- (if (region-active-p)
         (region-beginning)
       (car (bounds-of-thing-at-point 'symbol)))
     (line-beginning-position)))

(defun dumb-jump-get-lang-by-shell-contents (buffer)
  "Return languages in BUFFER by checking if file extension is mentioned."
  (let* ((buffer-contents (with-current-buffer buffer
                            (buffer-string)))

         (found (--filter (s-match (concat "\\." (plist-get it :ext) "\\b") buffer-contents)
                          dumb-jump-language-file-exts)))
    (--map (plist-get it :language) found)))

(defun dumb-jump-fetch-results (cur-file proj-root lang _config &optional prompt)
  "Search for symbol in PROMPT or symbol at point.

Return a list of results based on current file context and calling the
currently selected searcher tool (grep, ag, rg, ...).

CUR-FILE is the path of the current buffer.
PROJ-ROOT is that file's root project directory.
LANG is a string programming language with CONFIG a property list
of project configuration.
PROMPT is an optional string identifying item to search.
The returned property list has the following members:
- :results:
- :lang    : string: language name
- :symbol  : string: symbol being searched
- :ctx-type: string:
- :file    : string: file name
- :root    : string: project root."
  (let* ((cur-line-num (line-number-at-pos))
         (proj-config (dumb-jump-get-config proj-root))
         (config (when (string-suffix-p ".dumbjump" proj-config)
                   (dumb-jump-read-config proj-root proj-config)))
         (found-symbol (or prompt (dumb-jump-get-point-symbol)))
         (look-for (dumb-jump-process-symbol-by-lang lang found-symbol))
         (pt-ctx (if prompt
                     (get-text-property 0 :dumb-jump-ctx prompt)
                   (dumb-jump-get-point-context
                    (dumb-jump-get-point-line)
                    look-for
                    (dumb-jump--get-symbol-start))))
         (ctx-type
          (dumb-jump-get-ctx-type-by-language lang pt-ctx))

         (gen-funcs (dumb-jump-pick-grep-variant proj-root))
         (parse-fn (plist-get gen-funcs :parse)) ; dumb-jump-parse-TOOL-response
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
                       (dumb-jump-run-command look-for it
                                              regexes lang
                                              exclude-paths cur-file
                                              cur-line-num parse-fn
                                              generate-fn)
                       search-paths))

         (results (delete-dups (--map (plist-put it :target look-for) raw-results))))

    `(:results ,results
      :lang ,(if (null lang) "" lang)
      :symbol ,look-for
      :ctx-type ,(if (null ctx-type) "" ctx-type)
      :file ,cur-file
      :root ,proj-root)))

;;;###autoload
(defun dumb-jump-back ()
  "Jump back to where the last jump was done."
  ;; Note: made obsolete by Emacs `xref-pop-marker-stack'.
  (interactive)
  (with-demoted-errors "Error running `dumb-jump-before-jump-hook': %S"
    (run-hooks 'dumb-jump-before-jump-hook))
  (pop-tag-mark)
  (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
    (run-hooks 'dumb-jump-after-jump-hook)))

;;;###autoload
(defun dumb-jump-quick-look ()
  "Run `dumb-jump-go' in quick look mode.
That is, show a tooltip of where it would jump instead."
  ;; Note: made obsolete by Emacs xref interface.
  (interactive)
  (with-no-warnings
    (dumb-jump-go t)))

;;;###autoload
(defun dumb-jump-go-other-window ()
  "Like dumb-jump-go' but use `find-file-other-window' instead of `find-file'."
  ;; Note: made obsolete by Emacs xref interface.
  (interactive)
  (let ((dumb-jump-window 'other))
    (with-no-warnings
      (dumb-jump-go))))

;;;###autoload
(defun dumb-jump-go-current-window ()
  "Like `dumb-jump-go' but always use `find-file'."
  ;; Note: made obsolete by Emacs xref interface.
  (interactive)
  (let ((dumb-jump-window 'current))
    (with-no-warnings
      (dumb-jump-go))))

;;;###autoload
(defun dumb-jump-go-prefer-external ()
  "Like `dumb-jump-go' but prefer external matches from the current file."
  ;; Note: made obsolete by Emacs xref interface.
  (interactive)
  (with-no-warnings
    (dumb-jump-go nil t)))

;;;###autoload
(defun dumb-jump-go-prompt ()
  "Like `dumb-jump-go' but prompt for function instead of using under point."
  ;; Note: made obsolete by Emacs xref interface.
  (interactive)
  (with-no-warnings
    (dumb-jump-go nil nil (read-from-minibuffer "Jump to: "))))

;;;###autoload
(defun dumb-jump-go-prefer-external-other-window ()
  "Like `dumb-jump-go-prefer-external' but create another window.
It uses `find-file-other-window' instead of `find-file'."
  ;; Note: made obsolete by Emacs xref interface.
  (interactive)
  (let ((dumb-jump-window 'other))
    (with-no-warnings
      (dumb-jump-go-prefer-external))))

;;;###autoload
(defun dumb-jump-go (&optional use-tooltip prefer-external prompt)
  "Go to the function/variable declaration for thing at point.
When USE-TOOLTIP is t a tooltip jump preview will show instead.
When PREFER-EXTERNAL is t it will sort external matches before
current file.
PROMPT is an optional string: the name of the item to jump to."
  ;; Note: made obsolete by Emacs xref interface.
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
    (when (> fetch-time dumb-jump-max-find-time)
      (dumb-jump-message
       "Took over %ss to find '%s'. \
Please install ag or rg, or add a .dumbjump file to '%s' with path exclusions"
       (number-to-string dumb-jump-max-find-time) look-for proj-root))
    (cond
     ((eq issue 'nogrep)
      (dumb-jump-message "Please install ag, rg, git grep or grep!"))
     ((eq issue 'nosymbol)
      (dumb-jump-message "No symbol under point."))
     ((string-suffix-p " file" lang)
      (dumb-jump-message "Could not find rules for '%s'." lang))
     ((= result-count 1)
      (dumb-jump-result-follow (car results) use-tooltip proj-root))
     ((> result-count 1)
      ;; multiple results so let the user pick from a list
      ;; unless the match is in the current file
      (dumb-jump-handle-results results
                                (plist-get info :file)
                                proj-root
                                (plist-get info :ctx-type)
                                look-for use-tooltip prefer-external lang))
     ((= result-count 0)
      (dumb-jump-message "'%s' %s %s declaration not found."
                         look-for
                         (if (string-blank-p (or lang ""))
                             "with unknown language so"
                           lang)
                         (plist-get info :ctx-type))))))

(defcustom dumb-jump-language-comments
  '((:comment "//" :language "c++")
    (:comment "/*" :language "c++")
    (:comment ";"  :language "elisp")
    (:comment ";"  :language "commonlisp")
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
    (:comment ";"  :language "clojure")
    (:comment "#"  :language "coffeescript")
    (:comment "//" :language "faust")
    (:comment ";"  :language "fennel")
    (:comment "!"  :language "fortran")
    (:comment "//" :language "go")
    (:comment "//" :language "zig")
    (:comment "#"  :language "perl")
    (:comment "#"  :language "tcl")
    (:comment "//" :language "php")
    (:comment "#"  :language "php")
    (:comment "#"  :language "python")
    (:comment "%"  :language "matlab")
    (:comment "#"  :language "r")
    (:comment ";"  :language "racket")
    (:comment "#"  :language "ruby")
    (:comment "#"  :language "crystal")
    (:comment "#"  :language "nim")
    (:comment "#"  :language "nix")
    (:comment "//" :language "scala")
    (:comment ";"  :language "scheme")
    (:comment "#"  :language "janet")
    (:comment "#"  :language "shell")
    (:comment "//" :language "solidity")
    (:comment "//" :language "swift")
    (:comment "#"  :language "elixir")
    (:comment "%"  :language "erlang")
    (:comment "%"  :language "tex")
    (:comment "//" :language "systemverilog")
    (:comment "--" :language "vhdl")
    (:comment "//" :language "scss")
    (:comment "//" :language "pascal")
    (:comment "//" :language "protobuf")
    (:comment "#"  :language "hcl")
    (:comment "//" :language "apex")
    (:comment "*>" :language "cobol"))
  "List of one-line comments organized by language.
A language may have more than 1 comment string."
  :group 'dumb-jump
  :type
  '(repeat
    (plist
     :options ((:comment string)
               (:language string)))))

(defun dumb-jump-get-comment-by-language (lang)
  "Return a list of the one-line comment or comments for the given LANG."
  (let* ((entries (-distinct
                   (--filter (string= (plist-get it :language) lang)
                             dumb-jump-language-comments))))
    (if (> (length entries) 0)
        (mapcar (lambda (pl) (plist-get pl :comment)) entries)
      nil)))

(defun dumb-jump--context-startswith-comment (context-string comments)
  "Return t when CONTEXT-STRING starts with any COMMENTS string."
  (let ((startswith-comment nil)
        ;; trim leading white-space from context-string
        (context-str (replace-regexp-in-string "^\\s-+" "" context-string)))
    (catch 'found-one
      (dolist (comment comments)
        (when (string-prefix-p comment context-str)
          (setq startswith-comment t)
          (throw 'found-one nil))))
    startswith-comment))

(defun dumb-jump-filter-no-start-comments (results lang)
  "Filter out RESULTS with a :context starting with a LANG comment in file."
  (let ((comments (dumb-jump-get-comment-by-language lang)))
    (if comments
        (-concat
         (--filter (not (dumb-jump--context-startswith-comment
                         (plist-get it :context)
                         comments))
                   results))
      results)))

(defun dumb-jump-handle-results (results cur-file proj-root
                                         ctx-type look-for
                                         use-tooltip prefer-external
                                         &optional language)
  "Handle the searchers results.
RESULTS is a list of property lists with the searcher's results.
CUR-FILE is the current file within PROJ-ROOT.
CTX-TYPE is a string of the current context.
LOOK-FOR is the symbol we're jumping for.
USE-TOOLTIP shows a preview instead of jumping.
PREFER-EXTERNAL will sort current file last.
LANGUAGE is an optional language to pass to `dumb-jump-process-results'."
  (let* ((processed (dumb-jump-process-results results cur-file proj-root
                                               ctx-type look-for use-tooltip
                                               prefer-external language))
         (results (plist-get processed :results))
         (do-var-jump (plist-get processed :do-var-jump))
         (var-to-jump (plist-get processed :var-to-jump))
         (match-cur-file-front (plist-get processed :match-cur-file-front)))
    (dumb-jump-debug-message
     look-for
     ctx-type
     var-to-jump
     (pp-to-string match-cur-file-front)
     (pp-to-string results)
     prefer-external
     proj-root
     cur-file)
    (cond
     (use-tooltip ;; quick-look mode
      (popup-menu* (--map (dumb-jump--format-result proj-root it) results)))
     (do-var-jump
      (dumb-jump-result-follow var-to-jump use-tooltip proj-root))
     (t
      (dumb-jump-prompt-user-for-choice proj-root match-cur-file-front)))))

(defun dumb-jump-process-results (results cur-file proj-root ctx-type
                                          _look-for _use-tooltip
                                          prefer-external &optional language)
  "Process (filter, sort, ...) the searchers results.
RESULTS is a list of property lists with the searcher's results.
CUR-FILE is the current file within PROJ-ROOT.
CTX-TYPE is a string of the current context.
LOOK-FOR is the symbol we're jumping for.
USE-TOOLTIP shows a preview instead of jumping.
PREFER-EXTERNAL will sort current file last.
LANGUAGE is the optional given language, if nil it will be found by
`dumb-jump-get-language-by-filename'.
Figure which of the RESULTS to jump to.  Favoring the CUR-FILE."
  (let* ((lang (if language language
                 (dumb-jump-get-language-by-filename cur-file)))
         (match-sorted (-sort (lambda (x y)
                                (< (plist-get x :diff) (plist-get y :diff)))
                              results))
         (match-no-comments (dumb-jump-filter-no-start-comments match-sorted
                                                                lang))

         ;; Find the relative current file path by the project root.
         ;; In some cases the results will not be absolute but relative and
         ;; the "current file" filters must match in both cases. Also works
         ;; when current file is in an arbitrary sub folder.
         (rel-cur-file
          (cond ((and (string-prefix-p proj-root cur-file)
                      (string-prefix-p default-directory cur-file))
                 (substring cur-file
                            (length default-directory)
                            (length cur-file)))

                ((and (string-prefix-p proj-root cur-file)
                      (not (string-prefix-p default-directory cur-file)))
                 (substring cur-file
                            (1+ (length proj-root))
                            (length cur-file)))

                (t
                 cur-file)))

         ;; Moves current file results to the front of the list, unless
         ;; PREFER-EXTERNAL then put them last.
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

    (list :results results
          :do-var-jump do-var-jump
          :var-to-jump var-to-jump
          :match-cur-file-front match-cur-file-front)))

(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")

(defun dumb-jump-read-config (root config-file)
  "Load and return options (exclusions, inclusions, etc).
Ffrom the ROOT project CONFIG-FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name config-file root))
    (let ((local-root (if (file-remote-p root)
                          (progn
                            (require 'tramp)
                            (tramp-file-name-localname
                             (tramp-dissect-file-name root)))
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

(defun dumb-jump-file-modified-p (path)
  "Return non-nil if file at PATH is open in Emacs and has a modified buffer."
  (--any?
   (and (buffer-modified-p it)
        (buffer-file-name it)
        (file-exists-p (buffer-file-name it))
        (file-equal-p (buffer-file-name it) path))
   (buffer-list)))

(defun dumb-jump-result-follow (result &optional use-tooltip proj)
  "Jump to location identified by RESULT.
Before jumping, record the jump to be able to jump back later.

If destination buffer was modified, prompt user to proceed if the
`dumb-jump-confirm-jump-to-modified-file' user-option is non-nil,
otherwise display a warning.

With optional USE-TOOLTIP non-nil, pop a tool-tip showing the project
PROJ and RESULT information."
  (if (dumb-jump-file-modified-p (plist-get result :path))
      (let ((target-file (plist-get result :path)))
        (if dumb-jump-confirm-jump-to-modified-file
            (when (y-or-n-p
                   (concat
                    target-file
                    " has been modified so we may have the wrong location. Continue?"))
              (dumb-jump--result-follow result use-tooltip proj))
          (message
           "Warning: %s has been modified so we may have the wrong location."
           target-file)
          (dumb-jump--result-follow result use-tooltip proj)))
    (dumb-jump--result-follow result use-tooltip proj)))

(defun dumb-jump--result-follow (result &optional use-tooltip proj)
  "Jump to location identified by RESULT.
Before jumping, record the jump to be able to jump back later.

With optional USE-TOOLTIP non-nil, pop a tool-tip showing the project
PROJ and RESULT information."
  (let* ((target-boundary (s-matched-positions-all
                           (concat "\\b"
                                   (regexp-quote (plist-get result :target))
                                   "\\b")
                           (plist-get result :context)))
         ;; column position is either via tpos from ag or by using the regex
         ;; above or last using old s-index-of
         (column (if target-boundary
                     (car (car target-boundary))
                   (s-index-of (plist-get result :target)
                               (plist-get result :context))))

         (result-path (plist-get result :path))

         ;; Return value is either a string like "/ssh:user@1.2.3.4:" or nil
         (tramp-path-prefix (file-remote-p default-directory))

         ;; If result-path is an absolute path, the prefix is added to the head of it,
         ;; or result-path is added to the end of default-directory
         (path-for-tramp (when (and result-path tramp-path-prefix)
                           (if (file-name-absolute-p result-path)
                               (concat tramp-path-prefix result-path)
                             (concat default-directory result-path))))

         (thef (or path-for-tramp result-path))
         (line (plist-get result :line)))
    (when thef
      (if use-tooltip
          (popup-tip (dumb-jump--format-result proj result))
        (dumb-jump-goto-file-line thef line column)))
    ;; return the file for test
    thef))

(defun dumb-jump-goto-file-line (thefile theline column)
  "Open THEFILE, move point to line THELINE at COLUMN.
Push current position in the xref stack if available or inside
the old `find-tag-marker-ring' otherwise.
Run the `dumb-jump-before-jump-hook' hooks before jumping and
the `dumb-jump-after-jump-hook' hooks after the jump."
  ;; Push current position
  (if (fboundp 'xref-push-marker-stack)
      ;; On Emacs 25 and later: use xref
      (xref-push-marker-stack)
    ;; on older Emacs use the obsolete mechanism (but don't warn)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker))))
  ;; Run the before-jump hooks
  (with-demoted-errors "Error running `dumb-jump-before-jump-hook': %S"
    (run-hooks 'dumb-jump-before-jump-hook))
  ;; Open THEFILE in a buffer and a window specified by user-options.
  (let* ((visible-buffer (find-buffer-visiting thefile))
         (visible-window (when visible-buffer
                           (get-buffer-window visible-buffer))))
    (cond
     ((and visible-window dumb-jump-use-visible-window)
      (select-window visible-window))
     ((eq dumb-jump-window 'other)
      (find-file-other-window thefile))
     (t (find-file thefile))))
  ;; Move point to THELINE at COLUMN
  (goto-char (point-min))
  (forward-line (1- theline))
  (forward-char column)
  ;; Run the after-jump hooks
  (with-demoted-errors "Error running `dumb-jump-after-jump-hook': %S"
    (run-hooks 'dumb-jump-after-jump-hook)))

(defun dumb-jump-current-file-results (path results)
  "Return the PATH's RESULTS."
  (let ((matched (--filter (string= path (plist-get it :path)) results)))
    matched))

(defun dumb-jump-generators-by-searcher (searcher)
  "Return action property list for specific SEARCHER.

- SEARCHER: symbol: the name of the search tool.
            One of: git-grep, ag, git-grep-plus-ag, rg, gnu-grep, grep.
The returned plist has:
- :parse      : function: response parser.
- :generate   : function: search command string generator.  These functions
                          accept 6 arguments: look-for, cur-file, proj,
                                              regexes, lang, exclude-paths.
- :installed  : predicate installed? function: is searcher installed?
- :searcher   : symbol : the SEARCHER (see above)."
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
        ((equal 'git-grep-plus-ag searcher)
         `(:parse ,'dumb-jump-parse-ag-response
                  :generate ,'dumb-jump-generate-git-grep-plus-ag-command
                  :installed ,'dumb-jump-git-grep-plus-ag-installed?
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

(defun dumb-jump-selected-grep-variant (&optional proj-root)
  "Return search tool for current project or specified PROJ-ROOT.
Select search tool according to `dumb-jump-prefer-searcher' choice and its
possible `dumb-jump-force-searcher' overriding."
  (let ((searcher-found nil))
    (cond
     ;; If `dumb-jump-force-searcher' is not nil then use the searcher
     ;; identified by that user-option if possible.
     (dumb-jump-force-searcher
      (cond
       ;; Honour forced choices, they might be identified in dir-local file
       ((member dumb-jump-force-searcher '(ag
                                           rg
                                           grep
                                           gnu-grep
                                           git-grep
                                           git-grep-plus-ag))
        (setq searcher-found dumb-jump-force-searcher))
       ;; Select search tool identified by calling the user-specified function
       ;; which takes the directory.
       ((and (symbolp dumb-jump-force-searcher)
             (fboundp dumb-jump-force-searcher))
        (let ((user-selected-search-tool
               (funcall dumb-jump-force-searcher (or proj-root
                                                     (buffer-file-name)))))
          (when user-selected-search-tool
            (setq searcher-found user-selected-search-tool))))
       ;; Check if project root or current directory is inside one of the
       ;; project roots specified
       ((and (listp dumb-jump-force-searcher)
             proj-root
             (dumb-jump-git-grep-installed?)
             (member (dumb-jump-get-project-root proj-root)
                     dumb-jump-force-searcher)
             (file-exists-p (expand-file-name ".git" proj-root)))
        ;; [:todo 2026-01-07, by Pierre Rouleau: add support for git grep + ag??]
        (setq searcher-found 'git-grep)))))

    (unless searcher-found
      ;; When `dumb-jump-force-searcher' does not override the selection, then
      ;; select the searcher based on the value of `dumb-jump-force-searcher'.
      (cond
       ;; If `dumb-jump-prefer-searcher' identifies a searcher use it if it's
       ;; installed.
       ((and dumb-jump-prefer-searcher
             (funcall (plist-get (dumb-jump-generators-by-searcher
                                  dumb-jump-prefer-searcher)
                                 :installed)))
        (setq searcher-found dumb-jump-prefer-searcher))

       ;; Fallback searcher order.
       ((dumb-jump-ag-installed?)             (setq searcher-found 'ag))
       ((dumb-jump-rg-installed?)             (setq searcher-found 'rg))
       ((eq (dumb-jump-grep-installed?) 'gnu) (setq searcher-found 'gnu-grep))
       (t                                     (setq searcher-found 'grep))))
    ;; return the symbol identifying the selected search tool
    searcher-found))

(defun dumb-jump-pick-grep-variant (&optional proj-root)
  "Get action search property list for current project or specified PROJ-ROOT.
Select search tool according to `dumb-jump-prefer-searcher' choice and its
possible `dumb-jump-force-searcher' overriding."
  (dumb-jump-generators-by-searcher
   (dumb-jump-selected-grep-variant proj-root)))

(defun dumb-jump-shell-command-switch ()
  "Return current shell command switch prevent loading shell profile/RC.
Using that switch speeds shell command execution.
The `shell-file-name' identifies the current shell."
  (let ((base-name (downcase (file-name-base shell-file-name))))
    (cond
     ((or (string-equal "zsh" base-name)
          (string-equal "csh" base-name)
          (string-equal "tcsh" base-name))
      "-icf")
     ;;
     ((string-equal "bash" base-name)
      "-c")
     ;;
     (t
      shell-command-switch))))

(defconst dumb-jump--case-insensitive-languages
  '("commonlisp"
    "cobol")
  "List of case insensitive languages.")

;; TODO: rename dumb-jump-run-definition-command
(defun dumb-jump-run-command (look-for proj regexes lang
                                       exclude-args cur-file
                                       line-num parse-fn generate-fn)
  "Run the grep search command based on LOOK-FOR in the directory TOSEARCH.
The parameters are:
- LOOK-FOR: string: searched symbol
- PROJ:     string: directory path
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language name specified in `dumb-jump-find-rules'.
- EXCLUDE-ARGS:
- CUR-FILE  string: current file name.
- LINE-NUM: integer: current line number.
- PARSE-FN: the parse function; something like `dumb-jump-parse-grep-response'.
- GENERATE-FN: the generate function;
               something like `dumb-jump-generate-ag-command'."
  (let* ((proj-root (if (file-remote-p proj)
                        (directory-file-name
                         (tramp-file-name-localname (tramp-dissect-file-name proj)))
                      proj))
         (cmd (funcall generate-fn look-for cur-file proj-root regexes lang exclude-args))
         (shell-command-switch (dumb-jump-shell-command-switch))
         (rawresults (shell-command-to-string cmd)))

    (dumb-jump-debug-message cmd rawresults)
    (when (and (string-blank-p rawresults) dumb-jump-fallback-search)
      (setq regexes (list dumb-jump-fallback-regex))
      (setq cmd (funcall generate-fn
                         look-for cur-file
                         proj-root regexes
                         lang exclude-args))
      (setq rawresults (shell-command-to-string cmd))
      (dumb-jump-debug-message cmd rawresults))
    (unless (string-blank-p (or cmd ""))
      (let ((results (funcall parse-fn rawresults cur-file line-num))
            (ignore-case (member lang dumb-jump--case-insensitive-languages)))
        (--filter (s-contains? look-for
                               (plist-get it :context) ignore-case)
                  results)))))

(defun dumb-jump-parse-response-line (resp-line cur-file)
  "Parse a search program's single RESP-LINE for CUR-FILE.
Return a list of (path line context)."
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

;; --
;; Parse Search Tool Results: indirectly called by `dumb-jump-run-command'
;; -----------------------------------------------------------------------
;;   - `dumb-jump-parse-grep-response'
;;   - `dumb-jump-parse-ag-response'
;;   - `dumb-jump-parse-rg-response'
;;   - `dumb-jump-parse-git-grep-response'
;;     - `dumb-jump-parse-response-lines'

(defun dumb-jump-parse-response-lines (parsed cur-file cur-line-num)
  "Turn PARSED response lines into a list of property lists.
Using CUR-FILE and CUR-LINE-NUM to exclude jump origin."
  (let* ((records (--mapcat
                   (when it
                     (let* ((line-num (string-to-number (nth 1 it)))
                            (diff (- cur-line-num line-num)))
                       (list `(:path ,(nth 0 it)
                                     :line ,line-num
                                     :context ,(nth 2 it)
                                     :diff ,diff))))
                   parsed))
         (results (-non-nil records)))
    (--filter
     (not (and
           (string= (plist-get it :path) cur-file)
           (= (plist-get it :line) cur-line-num)))
     results)))

(defun dumb-jump-parse-grep-response (resp cur-file cur-line-num)
  "Parse raw grep search response RESP into a list of plists.
- CUR-FILE:     current file.
- CUR-LINE-NUM: current line number."
  (let* ((resp-no-warnings
          (--filter (and (not (string-prefix-p "grep:" it))
                         (not (s-contains? "No such file or" it)))
                    (s-split "\n" (s-trim resp))))
         (parsed (--map (dumb-jump-parse-response-line it cur-file)
                        resp-no-warnings)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-parse-ag-response (resp cur-file cur-line-num)
  "Parse raw ag search response RESP into a list of plists.
- CUR-FILE:     current file.
- CUR-LINE-NUM: current line number."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumb-jump-parse-response-line it cur-file)
                        resp-lines)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-parse-rg-response (resp cur-file cur-line-num)
  "Parse raw rg search response RESP into a list of plists.
- CUR-FILE:     current file.
- CUR-LINE-NUM: current line number."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumb-jump-parse-response-line it cur-file)
                        resp-lines)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

(defun dumb-jump-parse-git-grep-response (resp cur-file cur-line-num)
  "Parse raw git grep response RESP into a list of plists.
- CUR-FILE:     current file.
- CUR-LINE-NUM: current line number."
  (let* ((resp-lines (s-split "\n" (s-trim resp)))
         (parsed (--map (dumb-jump-parse-response-line it cur-file)
                        resp-lines)))
    (dumb-jump-parse-response-lines parsed cur-file cur-line-num)))

;; --

(defun dumb-jump-re-match (re s)
  "Does regular expression RE match string S. If RE is nil return nil."
  (when (and re s)
    (s-match re s)))

(defun dumb-jump-get-ctx-type-by-language (lang pt-ctx)
  "Detect the type of context by the language LANG and its context PT-CTX.
PT-CTX is assumed to be a property list with the :left and :right attributes
that are both strings."
  (let* ((contexts (--filter (string= (plist-get it ':language)
                                      lang)
                             dumb-jump-language-contexts))
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
  "Return the --include grep argument of file extensions for LANGUAGE."
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
  "Get list of search regular expressions by LANG and CTX-TYPE.
The arguments are:
- LANG:     string: the language.
- CTX-TYPE: variable, function, etc.
- SEARCHER: symbol: the name of the search tool.
            One of: git-grep, ag, git-grep-plus-ag, rg, gnu-grep, grep."
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
  "Populate IT regex template with LOOK-FOR for the specific search VARIANT.
Replace the dumb-jump generic regular expression meta concepts with the
one required for the search variant tool.
The arguments are:
- IT      : string: the dumb-jump generic regular expression.
- LOOK-FOR: string: the searched item.
- VARIANT : symbol: the search tool variant,
                    one of: ag, rg, grep, gnu-grep, git-grep-plus-ag, git-grep."
  (let ((boundary
         (cond ((eq variant 'rg) dumb-jump-rg-word-boundary)
               ((eq variant 'ag) dumb-jump-ag-word-boundary)
               ((eq variant 'git-grep-plus-ag) dumb-jump-ag-word-boundary)
               ((eq variant 'git-grep) dumb-jump-git-grep-word-boundary)
               (t dumb-jump-grep-word-boundary))))
    (let ((text it))
      (setq text (s-replace "\\j" boundary text))
      (when (dumb-jump-use-space-bracket-exp-for variant)
        (setq text (s-replace "\\s" "[[:space:]]" text)))
      (setq text (s-replace "JJJ" (regexp-quote look-for) text))
      text)))

(defun dumb-jump-populate-regexes (look-for regexes variant)
  "Return list of VARIANT specific regexes from the dumb-jump generic REGEXES.
Update the generic regexes, populate the target identified in LOOK-FOR.
The arguments are:
- LOOK-FOR: string: the searched item.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- VARIANT: symbol: the search tool variant,
                   one of: ag, rg, git-grep-plus-ag, git-grep."
  (--map
   (dumb-jump-populate-regex it look-for variant)
   regexes))

;; --
(defun dumb-jump-generate-ag-command (look-for
                                      cur-file proj
                                      regexes lang
                                      exclude-paths)
  "Return the ag command to search for LOOK-FOR in the PROJ directory.
The arguments are:
- LOOK-FOR: string: the searched item.
- CUR-FILE: string: current file name.  Used to check if it's compressed.
- PROJ: string: project root path.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language.
- EXCLUDE-PATHS: list of strings: directories to exclude from search."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'ag))
         (agtypes (dumb-jump-get-ag-type-by-language lang))
         (lang-exts (dumb-jump-get-file-exts-by-language lang))
         (proj-dir (file-name-as-directory proj))
         ;; TODO: --search-zip always? in case the include is the in gz area
         ;;       like emacs lisp code.
         (cmd (concat dumb-jump-ag-cmd
                      " --nocolor --nogroup"
                      (if (member lang dumb-jump--case-insensitive-languages)
                          " --ignore-case"
                        "")
                      (if (string-suffix-p ".gz" cur-file)
                          " --search-zip"
                        "")
                      (unless (string-blank-p dumb-jump-ag-search-args)
                        (concat " " dumb-jump-ag-search-args))
                      (if agtypes
                          (s-join "" (--map (format " --%s" it) agtypes))
                        ;; there can only be one `-G` arg
                        (concat " -G '("
                                (s-join "|"
                                        (--map (format "\\.%s" it) lang-exts))
                                ")$'"))))
         (exclude-args (dumb-jump-arg-joiner
                        "--ignore-dir"
                        (--map
                         (shell-quote-argument (s-replace proj-dir "" it))
                         exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (null regexes)
        ""
      (dumb-jump-concat-command cmd exclude-args "--" regex-args proj))))

;; --
(defun dumb-jump-get-git-grep-files-matching-symbol (symbol proj-root)
  "Return a list of file names in PROJ-ROOT holding the literal SYMBOL.
Search for matching files using git grep."
  (let* ((cmd (format "git grep --full-name -F -c %s %s"
                      (shell-quote-argument symbol) proj-root))
         (result (s-trim (shell-command-to-string cmd)))
         ;; result: '\n\ separated lines of: "path-name:count"
         ;; extract the name of files and return them in a list.
         (matched-files (--map
                         (cl-first (s-split ":" it))
                         (s-split "\n" result))))
    matched-files))

(defun dumb-jump-format-files-as-ag-arg (files proj-root)
  "Take a list of FILES and their PROJ-ROOT and return a `ag -G` argument."
  (format "'(%s)'" (s-join "|" (--map (file-relative-name
                                       (expand-file-name it proj-root))
                                      files))))

(defun dumb-jump-get-git-grep-files-matching-symbol-as-ag-arg (symbol proj-root)
  "Return optimized `ag -G` argument string to search for SYMBOL in PROJ-ROOT.
Limit the ag search to files matching SYMBOL in PROJ-ROOT by first getting the
list of files with `git grep`.
The string does not contain the `ag -G`, just its argument."
  (dumb-jump-format-files-as-ag-arg
   (dumb-jump-get-git-grep-files-matching-symbol symbol proj-root)
   proj-root))


;; git-grep plus ag only recommended for huge repos like the linux kernel.
(defun dumb-jump-generate-git-grep-plus-ag-command (look-for
                                                    cur-file proj
                                                    regexes lang
                                                    exclude-paths)
  "Return the ag command to search for LOOK-FOR in the PROJ directory.
Using ag to search only the files found via git-grep literal symbol search.
Note: Only recommended for huge repos like the linux kernel.
The arguments are:
- LOOK-FOR: string: the searched text
- CUR-FILE:
- PROJ: string: project root path.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language.
- EXCLUDE_PATHS: list of strings: directories to exclude from search."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'ag))
         (proj-dir (file-name-as-directory proj))
         (ag-files-arg
          (dumb-jump-get-git-grep-files-matching-symbol-as-ag-arg
           look-for proj-dir))
         (cmd (concat dumb-jump-ag-cmd
                      " --nocolor --nogroup"
                      (if (member lang dumb-jump--case-insensitive-languages)
                          " --ignore-case"
                        "")
                      (if (string-suffix-p ".gz" cur-file)
                          " --search-zip"
                        "")
                      " -G " ag-files-arg
                      " "))
         (exclude-args (dumb-jump-arg-joiner
                        "--ignore-dir"
                        (--map
                         (shell-quote-argument (s-replace proj-dir "" it))
                         exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (null regexes)
        ""
      (dumb-jump-concat-command cmd exclude-args regex-args proj))))

;; --
(defun dumb-jump-generate-rg-command (look-for
                                      _cur-file proj
                                      regexes lang
                                      exclude-paths)
  "Return the rg command to search for LOOK-FOR in the PROJ directory.
The arguments are:
- LOOK-FOR: string: the searched item.
- CUR-FILE:
- PROJ: string: project root path.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language.
- EXCLUDE-PATHS: list of strings: directories to exclude from search."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'rg))
         (rgtypes (dumb-jump-get-rg-type-by-language lang))
         (proj-dir (file-name-as-directory proj))
         (cmd (concat dumb-jump-rg-cmd
                      " --color never --no-heading --line-number -U"
                      (if (member lang dumb-jump--case-insensitive-languages)
                          " --ignore-case"
                        "")
                      (unless (string-blank-p dumb-jump-rg-search-args)
                        (concat " " dumb-jump-rg-search-args))
                      (s-join "" (--map (format " --type %s" it) rgtypes))))
         (exclude-args (dumb-jump-arg-joiner
                        "-g"
                        (--map
                         (shell-quote-argument
                          (concat "!" (s-replace proj-dir "" it)))
                         exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (null regexes)
        ""
      (dumb-jump-concat-command cmd exclude-args "--" regex-args proj))))

(defun dumb-jump-generate-git-grep-command (look-for
                                            cur-file proj
                                            regexes lang
                                            exclude-paths)
  "Return the git grep command to search for LOOK-FOR in the PROJ directory.
The arguments are:
- LOOK-FOR: string: the searched text
- CUR-FILE: string: current file name.  Used to check the file extension and
                    restrict search to file extensions of the matching LANG.
- PROJ: string: project root path.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language.
- EXCLUDE_PATHS:list of strings: directories to exclude from search."
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes 'git-grep))
         (ggtypes (when (file-name-extension cur-file)
                    (dumb-jump-get-git-grep-type-by-language lang)))
         (cmd (concat dumb-jump-git-grep-cmd
                      " --color=never --line-number"
                      (if (member lang dumb-jump--case-insensitive-languages)
                          " --ignore-case"
                        "")
                      (when dumb-jump-git-grep-search-untracked
                        " --untracked")
                      (unless (string-blank-p dumb-jump-git-grep-search-args)
                        (concat " " dumb-jump-git-grep-search-args))
                      " -E"))
         (fileexps (s-join " "
                           (or
                            (--map (shell-quote-argument
                                    (format "%s/*.%s" proj it))
                                   ggtypes)
                            '(":/"))))
         (exclude-args (s-join " "
                               (--map (shell-quote-argument
                                       (concat ":(exclude)" it))
                                      exclude-paths)))
         (regex-args (shell-quote-argument (s-join "|" filled-regexes))))
    (if (null regexes)
        ""
      (dumb-jump-concat-command cmd regex-args "--" fileexps exclude-args))))

(defun dumb-jump-generate-grep-command (look-for
                                        cur-file proj
                                        regexes lang
                                        exclude-paths)
  "Return the grep command to search for LOOK-FOR in PROJ directory.
The arguments are:
- LOOK-FOR: string: the searched item.
- CUR-FILE: string: current file name.  Used to check if it's compressed.
- PROJ: string: project root path.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language.
- EXCLUDE-PATHS: list of strings: directories to exclude from search."
  (let* ((filled-regexes
          (--map (shell-quote-argument it)
                 (dumb-jump-populate-regexes look-for regexes 'grep)))
         (cmd (concat
               (if (eq system-type 'windows-nt)
                   ""
                 (concat dumb-jump-grep-prefix " "))
               (if (string-suffix-p ".gz" cur-file)
                   dumb-jump-zgrep-cmd
                 dumb-jump-grep-cmd)))
         (case-args (if (member lang dumb-jump--case-insensitive-languages)
                        " --ignore-case"
                      ""))
         (exclude-args (dumb-jump-arg-joiner "--exclude-dir" exclude-paths))
         (include-args (dumb-jump-get-ext-includes lang))
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (null regexes)
        ""
      (dumb-jump-concat-command cmd dumb-jump-grep-args
                                case-args exclude-args
                                include-args regex-args proj))))

(defun dumb-jump-generate-gnu-grep-command (look-for
                                            cur-file proj
                                            regexes lang
                                            _exclude-paths)
  "Return the GNU grep command to search for LOOK-FOR in PROJ directory.
The arguments are:
- LOOK-FOR: string: the searched item.
- CUR-FILE: string: current file name.  Used to check if it's compressed.
- PROJ: string: project root path.
- REGEXES: list of strings: each string is a dumb-jump generic regular
                            expression.
- LANG: string: handled language.
- EXCLUDE-PATHS: list of strings: directories to exclude from search."
  (let* ((filled-regexes
          (--map (shell-quote-argument it)
                 (dumb-jump-populate-regexes look-for regexes 'gnu-grep)))
         (cmd (concat
               (if (eq system-type 'windows-nt)
                   ""
                 (concat dumb-jump-grep-prefix " "))
               (if (string-suffix-p ".gz" cur-file)
                   dumb-jump-zgrep-cmd
                 dumb-jump-grep-cmd)))
         (case-args (if (member lang dumb-jump--case-insensitive-languages)
                        " --ignore-case"
                      ""))
         ;; TODO: GNU grep doesn't support these, so skip them
         (exclude-args "")
         (include-args "")
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (null regexes)
        ""
      (dumb-jump-concat-command cmd dumb-jump-gnu-grep-args
                                case-args exclude-args
                                include-args regex-args proj))))

;; ---------------------------------------------------------------------------

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
  "Return a list of rules for the LANGUAGE by SEARCHER."
  (let* ((searcher-str (cond ((eq 'git-grep searcher) "git-grep")
                             ((eq 'rg searcher) "rg")
                             ((eq 'ag searcher) "ag")
                             ((eq 'git-grep-plus-ag searcher) "ag")
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
  "Minor mode for jumping to variable and function definitions."
  ;; Note: made obsolete by Emacs xref interface.
  :global t
  :keymap dumb-jump-mode-map)


;; -----------------------------------------------------------------------------
;;; Xref Backend

(when (featurep 'xref)
  (unless dumb-jump-disable-obsolete-warnings
    (dolist (obsolete
             '(dumb-jump-mode
               dumb-jump-go
               dumb-jump-go-prefer-external-other-window
               dumb-jump-go-prompt
               dumb-jump-quick-look
               dumb-jump-go-other-window
               dumb-jump-go-current-window
               dumb-jump-go-prefer-external
               dumb-jump-go-current-window))
      (make-obsolete
       obsolete
       (format "`%s' has been obsoleted by the xref interface."
               obsolete)
       "2020-06-26"))
    (make-obsolete 'dumb-jump-back
                   "`dumb-jump-back' has been obsoleted by `xref-pop-marker-stack'."
                   "2020-06-26"))

  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql dumb-jump)))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (and bounds (let* ((ident (dumb-jump-get-point-symbol))
			 (start (car bounds))
			 (col (- start (line-beginning-position)))
			 (line (dumb-jump-get-point-line))
			 (ctx (dumb-jump-get-point-context line ident col)))
		    (propertize ident :dumb-jump-ctx ctx)))))

  (cl-defmethod xref-backend-definitions ((_backend (eql dumb-jump)) prompt)
    (let* ((info (dumb-jump-get-results prompt))
           (results (plist-get info :results))
           (look-for (or prompt (plist-get info :symbol)))
           (proj-root (plist-get info :root))
           (issue (plist-get info :issue))
           (lang (plist-get info :lang))
           (processed (dumb-jump-process-results
                       results
                       (plist-get info :file)
                       proj-root
                       (plist-get info :ctx-type)
                       look-for
                       nil
                       nil
                       lang))
           (results (plist-get processed :results))
           (do-var-jump (plist-get processed :do-var-jump))
           (var-to-jump (plist-get processed :var-to-jump))
           (match-cur-file-front (plist-get processed :match-cur-file-front)))

      (dumb-jump-debug-message
        look-for
        (plist-get info :ctx-type)
        var-to-jump
        (pp-to-string match-cur-file-front)
        (pp-to-string results)
        match-cur-file-front
        proj-root
        (plist-get info :file))
      (cond ((eq issue 'nogrep)
             (dumb-jump-message "Please install ag, rg, git grep or grep!"))
            ((eq issue 'nosymbol)
             (dumb-jump-message "No symbol under point."))
            ((string-suffix-p " file" lang)
             (dumb-jump-message "Could not find rules for '%s'." lang))
            ((= (length results) 0)
             (dumb-jump-message "'%s' %s %s declaration not found."
                                look-for
                                (if (string-blank-p (or lang ""))
                                    "with unknown language so"
                                  lang)
                                (plist-get info :ctx-type)))
            (t (mapcar (lambda (res)
                         (with-no-warnings
                           (xref-make
                            (plist-get res :context)
                            (xref-make-file-location
                             (expand-file-name (plist-get res :path))
                             (plist-get res :line)
                             0))))
                       (if do-var-jump
                           (list var-to-jump)
                         match-cur-file-front))))))

  (cl-defmethod xref-backend-apropos ((_backend (eql dumb-jump)) pattern)
    (xref-backend-definitions 'dumb-jump pattern))
  (cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql dumb-jump)))
    nil))

;;;###autoload
(defun dumb-jump-xref-activate ()
  "Function to activate xref backend.
Add this function to `xref-backend-functions' to dumb jump to be
activated, whenever it finds a project.  It is recommended to add
it to the end, so that it only gets activated when no better
option is found."
  (and (dumb-jump-get-project-root default-directory)
       'dumb-jump))

(provide 'dumb-jump)
;;; dumb-jump.el ends here
