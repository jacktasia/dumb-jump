;;; -*- lexical-binding: t -*-
(require 'dumb-jump)
(require 'ert)
(require 'f)
(require 'dash)

(setq test-data-dir (f-expand "./test/data"))

(ert-deftest dumb-jump-dummy-test ()
  (should (string= (dumb-jump-asdf) "asdf")))

(ert-deftest dumb-jump-mode-to-language-test ()
  (should (-contains? (dumb-jump-get-languages-by-mode "emacs-lisp-mode") "elisp")))

(ert-deftest dumb-jump-language-to-mode-test ()
  (should (-contains? (dumb-jump-get-modes-by-language "elisp") "emacs-lisp-mode")))

(ert-deftest dumb-jump-get-rules-by-mode-test ()
  (should (= 3 (length (dumb-jump-get-rules-by-mode "emacs-lisp-mode")))))

(ert-deftest dumb-jump-generate-command-test ()
  (should (string= "LANG=C grep -REn \\(defun\\s+tester\\s+ -e \\(defvar\\s+tester\\s+ -e \\(setq\\s+tester\\s+" (dumb-jump-generate-command "emacs-lisp-mode" "tester"))))

(ert-deftest data-dir-exists-test ()
  (should (f-dir? test-data-dir)))
