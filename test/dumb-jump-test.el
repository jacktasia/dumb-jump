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

(ert-deftest dumb-jump-get-rules-by-mode-test ()
  (should (= 3 (length (dumb-jump-get-rules-by-mode "emacs-lisp-mode")))))

(ert-deftest dumb-jump-generate-command-no-ctx-test ()
  (let ((expected "LANG=C grep -REn -e '\\(defun\\s+tester\\s*' -e '\\(defvar\\b\\s*tester\\b\\s*' -e '\\(setq\\b\\s*tester\\b\\s*' ."))
    (should (string= expected  (dumb-jump-generate-command "emacs-lisp-mode" "tester" "." nil)))))

(ert-deftest dumb-jump-generate-command-with-ctx-test ()
  (let ((expected "LANG=C grep -REn -e '\\(defun\\s+tester\\s*' ."))
    ;; the point context being passed should match a "function" type so only the one command
    (should (string= expected  (dumb-jump-generate-command "emacs-lisp-mode" "tester" "." '(:left "(" :right nil))))))

(ert-deftest dumb-jump-generate-bad-command-test ()
    (should (s-blank? (dumb-jump-generate-command "unknown-mode" "tester" "." nil))))

(ert-deftest dumb-jump-grep-parse-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar dumb-jump-grep-prefix )\n./dumb-jump.el:28:(defvar dumb-jump-grep)")
         (parsed (dumb-jump-parse-grep-response resp)))
    (should (string= (plist-get (nth 1 parsed) ':line) "26"))))


(ert-deftest dumb-jump-run-cmd-test ()
  (let* ((results (dumb-jump-run-command "emacs-lisp-mode" "another-fake-function" test-data-dir-elisp nil))
        (first-result (car results)))
    (should (s-contains? "/fake.el" (plist-get first-result :path)))
    (should (string= (plist-get first-result :line) "6"))))

(ert-deftest dumb-jump-run-cmd-fail-test ()
  (let* ((results (dumb-jump-run-command "emacs-lisp-mode" "hidden-function" test-data-dir-elisp nil))
        (first-result (car results)))
    (should (null first-result))))

(ert-deftest dumb-jump-find-proj-root-test ()
  (let* ((js-file (f-join test-data-dir-proj1 "src" "js"))
         (found-project (dumb-jump-get-project-root js-file)))
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

(ert-deftest dumb-jump-context-point-test ()
  (let* ((sentence "mainWindow.loadUrl('file://' + __dirname + '/dt/inspector.html?electron=true');")
         (func "loadUrl")
         (pt-ctx (dumb-jump-get-point-context sentence func))
         (ctx-type (dumb-jump-get-ctx-type-by-mode "js2-mode" pt-ctx)))
    (should (string= ctx-type "function"))))
