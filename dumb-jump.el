;;; dumb-jump.el --- Dumb jumping to declarations

;; Copyright (C) 2015 jack angers
;; Author: jack angers
;; Version: 1.0
;; Package-Requires: ((json "1.2") (ht "2.0") (s "1.9.0") (dash "2.9.0") (cl-lib "0.5"))
;; Keywords: programming
;;; Commentary:

;; Uses `grep` to jump to delcartions via a list of regular expressions based on the major mode you are in.

;;; Code:
;; (require 'json)
;; (require 'url)
;; (require 'ht)
;; (require 's)
;; (require 'pp)
;; (require 'cl-lib)
(require 's)
(require 'dash)

;; TODO: document defvars
(defvar dumb-jump-grep-prefix "LANG=C grep")

(defvar dumb-jump-grep-args "-REn")

;; todo: ensure
(defvar dumb-jump-find-rules '((:type "function" :language "elisp" :regex "\\\(defun\\s+JJJ\\s+")
                               (:type "variable" :language "elisp" :regex "\\\(defvar\\s+JJJ\\s+")
                               (:type "variable" :language "elisp" :regex "\\\(setq\\s+JJJ\\s+")))

(defvar dumb-jump-language-modes '((:language "elisp" :mode "emacs-lisp-mode")))


(defun dumb-jump-run-command (mode lookfor tosearch)
  (let* ((cmd (dumb-jump-generate-command mode lookfor tosearch))
         (rawresults (shell-command-to-string cmd)))
    ;; (message cmd)
    ;; (message rawresults)
    (dumb-jump-parse-grep-response rawresults)))


;; TODO: put into a plist to treat as a map?
(defun dumb-jump-parse-grep-response (resp)
  (let ((parsed (-map (lambda (line) (s-split ":" line)) (s-split "\n" resp))))
    (-mapcat
      (lambda (x)
        (let ((item '()))
          (setq item (plist-put item :path (nth 0 x)))
          (setq item (plist-put item :line (nth 1 x)))
          (setq item (plist-put item :context (nth 2 x)))
          (list item)))
      parsed)))

(defun dumb-jump-generate-command (mode lookfor tosearch)
  (let* ((rules (dumb-jump-get-rules-by-mode mode))
         (regexes (-map (lambda (r) (format "'%s'" (plist-get r ':regex))) rules))
         (meat (s-join " -e " (-map (lambda (x) (s-replace "JJJ" lookfor x)) regexes))))
    (concat dumb-jump-grep-prefix " " dumb-jump-grep-args " -e " meat " " tosearch)))

(defun dumb-jump-get-rules-by-languages (languages)
  "Get a list of rules with a list of languages"
  (-mapcat (lambda (lang) (dumb-jump-get-rules-by-language lang)) languages))

(defun dumb-jump-get-rules-by-mode (mode)
  "Get a list of rules by a major mode"
  (dumb-jump-get-rules-by-languages (dumb-jump-get-languages-by-mode mode)))

(defun dumb-jump-get-rules-by-language (language)
  "Get list of rules for a language"
  (-filter (lambda (x) (string= (plist-get x ':language) language)) dumb-jump-find-rules))

(defun dumb-jump-get-modes-by-language (language)
  "Get all modes connected to a language"
  (-map (lambda (x) (plist-get x ':mode))
        (-filter (lambda (x) (string= (plist-get x ':language) language)) dumb-jump-language-modes)))

(defun dumb-jump-get-languages-by-mode (mode)
  "Get all languages connected to a mode"
  (-map (lambda (x) (plist-get x ':language))
        (-filter (lambda (x) (string= (plist-get x ':mode) mode)) dumb-jump-language-modes)))

;; for parsing a grep line
;;(-map (lambda (x) (s-split ":" x)) (s-split "\n" "a:1\nb:2\nc:c3"))


(provide 'dumb-jump)
;;; dumb-jump.el ends here
