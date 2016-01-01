;;; -*- lexical-binding: t -*-
(require 'dumb-jump)
(require 'ert)
(require 'f)
(require 's)
(require 'dash)

(setq test-data-dir (f-expand "./test/data"))
(setq test-data-dir-elisp (f-join test-data-dir "proj2-elisp"))
(setq test-data-dir-proj1 (f-join test-data-dir "proj1"))

(ert-deftest data-dir-exists-test ()
  (should (f-dir? test-data-dir)))

(ert-deftest data-dir-proj2-exists-test ()
  (should (f-dir? test-data-dir-elisp)))

(ert-deftest dumb-jump-mode-to-language-test ()
  (should (-contains? (dumb-jump-get-languages-by-mode "emacs-lisp-mode") "elisp")))

(ert-deftest dumb-jump-language-to-mode-test ()
  (should (-contains? (dumb-jump-get-modes-by-language "elisp") "emacs-lisp-mode")))

(ert-deftest dumb-jump-current-file-result-test ()
  (let ((results '((:path "blah") (:path "rarr")))
        (expected '(:path "blah")))
    (should (equal (dumb-jump-current-file-result "blah" results) expected))))

(ert-deftest dumb-jump-language-to-ext-test ()
  (should (-contains? (dumb-jump-get-file-exts-by-language "elisp") "el")))

(ert-deftest dumb-jump-get-rules-by-mode-test ()
  (should (= 3 (length (dumb-jump-get-rules-by-mode "emacs-lisp-mode")))))

(ert-deftest dumb-jump-generate-cmd-include-args ()
  (let ((args (dumb-jump-get-ext-includes "javascript"))
        (expected " --include \\*.js --include \\*.jsx --include \\*.html "))
    (should (string= expected args))))

(ert-deftest dumb-jump-generate-command-no-ctx-test ()
  (let ((regexes (dumb-jump-get-contextual-regexes "emacs-lisp-mode" nil))
        (expected "LANG=C grep -REn -e '\\(defun\\s+tester\\s*' -e '\\(defvar\\b\\s*tester\\b\\s*' -e '\\(setq\\b\\s*tester\\b\\s*' ."))
    (should (string= expected  (dumb-jump-generate-command  "tester" "." regexes "")))))

(ert-deftest dumb-jump-generate-command-with-ctx-test ()
  (let* ((ctx-type (dumb-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
        (regexes (dumb-jump-get-contextual-regexes "emacs-lisp-mode" ctx-type))
        (expected "LANG=C grep -REn -e '\\(defun\\s+tester\\s*' ."))
    ;; the point context being passed should match a "function" type so only the one command
    (should (string= expected  (dumb-jump-generate-command "tester" "." regexes "")))))

(ert-deftest dumb-jump-generate-bad-command-test ()
    (should (s-blank? (dumb-jump-generate-command "tester" "." nil ""))))

(ert-deftest dumb-jump-grep-parse-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar dumb-jump-grep-prefix )\n./dumb-jump.el:28:(defvar dumb-jump-grep)")
         (parsed (dumb-jump-parse-grep-response resp)))
    (should (string= (plist-get (nth 1 parsed) ':line) "26"))))

(ert-deftest dumb-jump-run-cmd-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "emacs-lisp-mode" nil))
         (results (dumb-jump-run-command "another-fake-function" test-data-dir-elisp regexes ""))
        (first-result (car results)))
    (should (s-contains? "/fake.el" (plist-get first-result :path)))
    (should (string= (plist-get first-result :line) "6"))))

(ert-deftest dumb-jump-run-cmd-fail-test ()
  (let* ((results (dumb-jump-run-command "hidden-function" test-data-dir-elisp nil ""))
        (first-result (car results)))
    (should (null first-result))))

(ert-deftest dumb-jump-find-proj-root-test ()
  (let* ((js-file (f-join test-data-dir-proj1 "src" "js"))
         (proj-info (dumb-jump-get-project-root js-file))
         (found-project (plist-get proj-info :root)))
    (should (f-exists? found-project))
    (should (string= found-project test-data-dir-proj1))))

(ert-deftest dumb-jump-goto-file-line-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (dumb-jump-goto-file-line js-file "3")
    (should (string= (buffer-file-name) js-file))
    (should (= (line-number-at-pos) 3))))

(ert-deftest dumb-jump-test-rules-test ()
  (let ((rule-failures (dumb-jump-test-rules)))
    (should (= (length rule-failures) 0))))

(ert-deftest dumb-jump-test-rules-fail-test ()
  (let* ((bad-rule '(:type "variable" :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
         (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
         (rule-failures (dumb-jump-test-rules)))
    ;(message "%s" (prin1-to-string rule-failures))
    (should (= (length rule-failures) 1))))

(ert-deftest dumb-jump-context-point-test ()
  (let* ((sentence "mainWindow.loadUrl('file://' + __dirname + '/dt/inspector.html?electron=true');")
         (func "loadUrl")
         (ctx (dumb-jump-get-point-context sentence func)))
         (should (string= (plist-get ctx :left) "."))
         (should (string= (plist-get ctx :right) "("))))

(ert-deftest dumb-jump-context-point-type-test ()
  (let* ((sentence "mainWindow.loadUrl('file://' + __dirname + '/dt/inspector.html?electron=true');")
         (func "loadUrl")
         (pt-ctx (dumb-jump-get-point-context sentence func))
         (ctx-type (dumb-jump-get-ctx-type-by-language "javascript" pt-ctx)))
    (should (string= ctx-type "function"))))

(ert-deftest dumb-jump-multiple-choice-input-test ()
  (progn
    (should (= (dumb-jump-parse-input 5 "4") 4))
    (should (= (dumb-jump-parse-input 50 "1") 1))
    (should (null (dumb-jump-parse-input 50 "242")))
    (should (null (dumb-jump-parse-input 5 "0")))
    (should (null (dumb-jump-parse-input 500 "asdf")))
    (should (null (dumb-jump-parse-input 5 "6")))))

(ert-deftest dumb-jump-multiple-choice-text-test ()
  (let* ((choice-txt (dumb-jump-generate-prompt-text "asdf" "/usr/blah" '((:path "/usr/blah/test.txt" :line "54"))))
         (expected "Multiple results for 'asdf':\n\n1. /test.txt:54\n\nChoice: "))
    (should (string= choice-txt expected))))
