;;; -*- lexical-binding: t -*-
(require 'f)
(require 's)
(require 'dash)
(require 'noflet)
(require 'el-mock)
(require 'popup)

(setq test-data-dir (f-expand "./test/data"))
(setq test-data-dir-elisp (f-join test-data-dir "proj2-elisp"))
(setq test-data-dir-proj1 (f-join test-data-dir "proj1"))

(ert-deftest data-dir-exists-test ()
  (should (f-dir? test-data-dir)))

(ert-deftest data-dir-proj2-exists-test ()
  (should (f-dir? test-data-dir-elisp)))

(ert-deftest dumb-jump-get-lang-by-ext-test ()
  (let ((lang1 (dumb-jump-get-language-by-filename "sldkfj.el"))
        (lang1b (dumb-jump-get-language-by-filename "sldkfj.el.gz"))
        (lang2 (dumb-jump-get-language-by-filename "/askdfjkl/somefile.js"))
        (nolang (dumb-jump-get-language-by-filename "/blah/somefile.bin")))
    (should (string= lang1 "elisp"))
    (should (string= lang1b "elisp"))
    (should (string= lang2 "javascript"))
    (should (null nolang))))

(ert-deftest dumb-jump-get-lang-major-mode-test ()
  (let* ((major-mode 'php)
         (lang1 (dumb-jump-get-language "blah/file.install"))
         (lang1b (dumb-jump-get-language-from-mode))
         (lang2 (dumb-jump-get-language-by-filename "/askdfjkl/somefile.js")))
    (should (string= lang1 "php"))
    (should (string= lang1b "php"))
    (should (string= lang2 "javascript"))))

(ert-deftest dumb-jump-current-files-results-test ()
  (let ((results '((:path "blah") (:path "rarr")))
        (expected '((:path "blah"))))
    (should (equal (dumb-jump-current-file-results "blah" results) expected))))

(ert-deftest dumb-jump-exclude-path-test ()
  (let* ((expected (list (f-join test-data-dir-proj1 "ignored")
                         (f-join test-data-dir-proj1 "ignored2")))
         (root (dumb-jump-get-project-root test-data-dir-proj1))
         (config (dumb-jump-read-config test-data-dir-proj1  ".dumbjump")))
    (should (equal (plist-get config :exclude)  expected))))

(ert-deftest dumb-jump-include-path-test ()
  (let* ((config (dumb-jump-read-config test-data-dir-proj1 ".dumbjump-include"))
         (expected (list (f-join test-data-dir-proj1 "../fake-library")
                         "/etc/var/some/code")))
    (should (equal (plist-get config :include) expected))))

(ert-deftest dumb-jump-exclude-path-blank-test ()
  (let* ((config (dumb-jump-read-config test-data-dir-proj1 ".dumbjump-blank")))
    (should (null (plist-get config :exclude)))
    (should (null (plist-get config :include)))))

(ert-deftest dumb-jump-config-lang-test ()
  (let* ((config (dumb-jump-read-config test-data-dir-proj1 ".dumbjump-lang")))
    (should (string= "python" (plist-get config :language)))))

(ert-deftest dumb-jump-language-to-ext-test ()
  (should (-contains? (dumb-jump-get-file-exts-by-language "elisp") "el")))

(ert-deftest dumb-jump-generate-cmd-include-args ()
  (let ((args (dumb-jump-get-ext-includes "javascript"))
        (expected " --include \\*.js --include \\*.jsx --include \\*.html "))
    (should (string= expected args))))

(ert-deftest dumb-jump-generate-grep-command-no-ctx-test ()
  (let ((regexes (dumb-jump-get-contextual-regexes "elisp" nil))
        (expected "LANG=C grep -REn --include \\*.el --include \\*.el.gz -e '\\(defun\\s+tester($|[^\\w-])' -e '\\(defvar\\b\\s*tester($|[^\\w-])' -e '\\(defcustom\\b\\s*tester($|[^\\w-])' -e '\\(setq\\b\\s*tester($|[^\\w-])' -e '\\(tester\\s+' -e '\\(defun\\s*.+\\(?\\s*tester($|[^\\w-])\\s*\\)?' ."))
    (should (string= expected  (dumb-jump-generate-grep-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-ag-command-no-ctx-test ()
  (let ((regexes (dumb-jump-get-contextual-regexes "elisp" nil))
        (expected "ag --nocolor --nogroup --elisp \"\\(defun\\s+tester(?![\\w-])|\\(defvar\\b\\s*tester(?![\\w-])|\\(defcustom\\b\\s*tester(?![\\w-])|\\(setq\\b\\s*tester(?![\\w-])|\\(tester\\s+|\\(defun\\s*.+\\(?\\s*tester(?![\\w-])\\s*\\)?\" ."))
    (should (string= expected  (dumb-jump-generate-ag-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-grep-command-no-ctx-funcs-only-test ()
  (let* ((dumb-jump-functions-only t)
        (regexes (dumb-jump-get-contextual-regexes "elisp" nil))
        (expected "LANG=C grep -REn -e '\\(defun\\s+tester($|[^\\w-])' .")
        (zexpected "LANG=C zgrep -REn -e '\\(defun\\s+tester($|[^\\w-])' ."))
    (should (string= expected  (dumb-jump-generate-grep-command  "tester" "blah.el" "." regexes "" nil)))
    (should (string= zexpected  (dumb-jump-generate-grep-command  "tester" "blah.el.gz" "." regexes "" nil)))))

(ert-deftest dumb-jump-generate-grep-command-with-ctx-test ()
  (let* ((ctx-type (dumb-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
         (dumb-jump-ignore-context nil) ;; overriding the default
         (regexes (dumb-jump-get-contextual-regexes "elisp" ctx-type))
         (expected "LANG=C grep -REn -e '\\(defun\\s+tester($|[^\\w-])' ."))
    ;; the point context being passed should match a "function" type so only the one command
    (should (string= expected  (dumb-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil)))))

(ert-deftest dumb-jump-generate-grep-command-with-ctx-but-ignored-test ()
  (let* ((ctx-type (dumb-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
         (dumb-jump-ignore-context t)
         (regexes (dumb-jump-get-contextual-regexes "elisp" ctx-type))
         (expected "LANG=C grep -REn -e '\\(defun\\s+tester($|[^\\w-])' -e '\\(defvar\\b\\s*tester($|[^\\w-])' -e '\\(defcustom\\b\\s*tester($|[^\\w-])' -e '\\(setq\\b\\s*tester($|[^\\w-])' -e '\\(tester\\s+' -e '\\(defun\\s*.+\\(?\\s*tester($|[^\\w-])\\s*\\)?' ."))

    ;; the point context being passed is ignored so ALL should return
    (should (string= expected  (dumb-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil)))))

(ert-deftest dumb-jump-generate-bad-grep-command-test ()
    (should (s-blank? (dumb-jump-generate-grep-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumb-jump-generate-bad-ag-command-test ()
    (should (s-blank? (dumb-jump-generate-ag-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumb-jump-grep-parse-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar some-var )\n./dumb-jump2.el:28:(defvar some-var)")
         (parsed (dumb-jump-parse-grep-response resp "dumb-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumb-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumb-jump-grep-parse-no-filter-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar some-var )\n")
         (parsed (dumb-jump-parse-grep-response resp "dumb-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumb-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumb-jump-ag-parse-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar some-var )\n./dumb-jump2.el:28:1:(defvar some-var)")
         (parsed (dumb-jump-parse-ag-response resp "dumb-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumb-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumb-jump-run-cmd-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil))
         (results (dumb-jump-run-command "another-fake-function" test-data-dir-elisp regexes "" ""  "blah.el" 3))
        (first-result (car results)))
    (should (s-contains? "/fake.el" (plist-get first-result :path)))
    (should (= (plist-get first-result :line) 6))))

(ert-deftest dumb-jump-run-grep-cmd-test ()
  (let* ((dumb-jump-force-grep t)
         (regexes (dumb-jump-get-contextual-regexes "elisp" nil))
         (results (dumb-jump-run-command "another-fake-function" test-data-dir-elisp regexes "" ""  "blah.el" 3))
        (first-result (car results)))
    (should (s-contains? "/fake.el" (plist-get first-result :path)))
    (should (= (plist-get first-result :line) 6))))

(ert-deftest dumb-jump-run-cmd-fail-test ()
  (let* ((results (dumb-jump-run-command "hidden-function" test-data-dir-elisp nil "" "" "blah.el" 3))
        (first-result (car results)))
    (should (null first-result))))

(ert-deftest dumb-jump-find-proj-root-test ()
  (let* ((js-file (f-join test-data-dir-proj1 "src" "js"))
         (found-project (dumb-jump-get-project-root js-file)))
    (should (f-exists? found-project))
    (should (string= found-project test-data-dir-proj1))
    (should (string= ".dumbjump" (dumb-jump-get-config found-project)))))

(ert-deftest dumb-jump-find-proj-root-default-test ()
  (with-mock (mock (locate-dominating-file * *))
    (let ((found-project (dumb-jump-get-project-root ""))
          (expected (f-expand dumb-jump-default-project)))
      (should (string= found-project expected)))))

(ert-deftest dumb-jump-goto-file-line-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (dumb-jump-goto-file-line js-file 3 0)
    (should (string= (buffer-file-name) js-file))
    (should (= (line-number-at-pos) 3))))

(ert-deftest dumb-jump-goto-file-point-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (dumb-jump-goto-file-point js-file 10)
    (should (string= (buffer-file-name) js-file))
    (should (= (point) 10))))

(ert-deftest dumb-jump-test-rules-test ()
  (let ((rule-failures (dumb-jump-test-rules)))
    (should (= (length rule-failures) 0))))

(ert-deftest dumb-jump-test-ag-rules-test ()
  (let ((rule-failures (dumb-jump-test-ag-rules)))
    (should (= (length rule-failures) 0))))

(ert-deftest dumb-jump-test-rules-not-test () ;; :not tests
  (let ((rule-failures (dumb-jump-test-rules t)))
    (should (= (length rule-failures) 0))))

(ert-deftest dumb-jump-test-ag-rules-not-test () ;; :not tests
  (let ((rule-failures (dumb-jump-test-ag-rules t)))
    (should (= (length rule-failures) 0))))

(ert-deftest dumb-jump-test-rules-fail-test ()
  (let* ((bad-rule '(:type "variable" :supports ("ag" "grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
         (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
         (rule-failures (dumb-jump-test-rules)))
    ;(message "%s" (prin1-to-string rule-failures))
    (should (= (length rule-failures) 1))))

(ert-deftest dumb-jump-test-ag-rules-fail-test ()
  (let* ((bad-rule '(:type "variable" :supports ("ag" "grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
         (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
         (rule-failures (dumb-jump-test-ag-rules)))
    ;(message "%s" (prin1-to-string rule-failures))
    (should (= (length rule-failures) 1))))

(ert-deftest dumb-jump-match-test ()
  (should (not (dumb-jump-re-match nil "asdf")))
  (should (dumb-jump-re-match "^asdf$" "asdf"))
  (should (string= (car (dumb-jump-re-match "^[0-9]+$" "123")) "123")))

(ert-deftest dumb-jump-context-point-test ()
  (let* ((sentence "mainWindow.loadUrl('file://')")
         (func "loadUrl")
         (ctx (dumb-jump-get-point-context sentence func 15)))
         (should (string= (plist-get ctx :left) "mainWindow."))
         (should (string= (plist-get ctx :right) "('file://')"))))

(ert-deftest dumb-jump-context-point-type-test ()
  (let* ((sentence "mainWindow.loadUrl('file://' + __dirname + '/dt/inspector.html?electron=true');")
         (func "loadUrl")
         (pt-ctx (dumb-jump-get-point-context sentence func 14))
         (ctx-type (dumb-jump-get-ctx-type-by-language "javascript" pt-ctx)))
    (should (string= ctx-type "function"))))

(ert-deftest dumb-jump-prompt-user-for-choice-correct-test ()
  (let* ((results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()") (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (popup-menu* *) => "/test2.txt:52 var thing = function()")
     (mock (dumb-jump-result-follow '(:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a")))
     (dumb-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumb-jump-fetch-results-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 10)
      (let ((results (dumb-jump-fetch-file-results)))
        (should (string= "doSomeStuff" (plist-get results :symbol)))
        (should (string= "javascript" (plist-get results :lang)))))))

(ert-deftest dumb-jump-go-shell-test ()
  (let* ((go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js"))
         (default-directory test-data-dir-proj1))
    (with-current-buffer (get-buffer-create "*shell*")
      (insert ".js doSomeStuff()")
      (goto-char (point-min))
      (forward-char 6)
      (let ((results (dumb-jump-get-results)))
        (should (string= "doSomeStuff" (plist-get results :symbol)))
        (should (string= "javascript" (plist-get results :lang)))))))

(ert-deftest dumb-jump-go-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (dumb-jump-goto-file-line * 3 9))
       (should (string= go-js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-quick-look-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (popup-tip "/src/js/fake.js:3 function doSomeStuff() {"))
       (should (string= go-js-file (dumb-jump-quick-look)))))))

(ert-deftest dumb-jump-go-js2-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 11)
      (forward-char 76)
      (with-mock
       (mock (dumb-jump-goto-file-line * 7 35))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-js-es6a-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (with-mock
       (mock (dumb-jump-goto-file-line * 1 4))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-js-es6b-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 21)
      (with-mock
       (mock (dumb-jump-goto-file-line * 3 6))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-js-es6c-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (with-mock
       (mock (dumb-jump-goto-file-line * 5 6))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-js-es6d-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 23)
      (with-mock
       (mock (dumb-jump-goto-file-line * 10 2))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-js-es6e-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 24)
      (with-mock
       (mock (dumb-jump-goto-file-line * 16 2))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-js-es6-class-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 36)
      (forward-char 12)
      (with-mock
       (mock (dumb-jump-goto-file-line * 28 6))
       (should (string= js-file (dumb-jump-go)))))))


(ert-deftest dumb-jump-go-sig-def-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 7)
      (forward-char 35)
      (with-mock
       (mock (dumb-jump-goto-file-line * 6 25))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-sig-def2-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 35)
      (with-mock
       (mock (dumb-jump-goto-file-line * 12 32))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-sig-def3-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (forward-char 35)
      (with-mock
       (mock (dumb-jump-goto-file-line * 19 32))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-var-let-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 33)
      (with-mock
       (mock (dumb-jump-goto-file-line * 11 10))
       (should (string= el-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-var-let-repeat-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 21)
      (forward-char 33)
      (with-mock
       (mock (dumb-jump-goto-file-line * 18 10))
       (should (string= el-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-go-var-arg-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 12)
      (with-mock
       (mock (dumb-jump-goto-file-line * 3 27))
       (should (string= el-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-a-back-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (dumb-jump-go)
       (mock (dumb-jump-goto-file-point * 14))
       (mock (dumb-jump-message "Jumping back to%s line %s" " fake2.js" "1"))
       (dumb-jump-back)))))

(ert-deftest dumb-jump-back-no-result-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (with-mock
       (mock (dumb-jump-message "Nowhere to jump back to."))
       (dumb-jump-back)))))

(ert-deftest dumb-jump-go-no-result-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (with-mock
       (mock (dumb-jump-message "'%s' %s %s declaration not found." "console" * *))
       (dumb-jump-go)))))

(ert-deftest dumb-jump-go-no-rules-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt")))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (with-mock
       (mock (dumb-jump-message "Could not find rules for '%s'." ".txt file"))
       (dumb-jump-go)))))

(ert-deftest dumb-jump-go-too-long-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt"))
        (dumb-jump-max-find-time 0.2))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (noflet ((dumb-jump-fetch-file-results ()
                                        (sleep-for 0 300)
                                        '()))
               (with-mock
                (mock (dumb-jump-message "Took over %ss to find '%s'. Please install ag or add a .dumbjump file to '%s' with path exclusions" * * *))
                (dumb-jump-go))))))

(ert-deftest dumb-jump-message-handle-results-test ()
  (let ((results '((:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-goto-file-line "src/file.js" 62 4))
     (dumb-jump-handle-results results "src/file.js" "/code/redux" "" "isNow" nil))))

(ert-deftest dumb-jump-message-handle-results-choices-test ()
  (let ((results '((:path "src/file2.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file2.js" :line 63 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file2.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-prompt-user-for-choice "/code/redux" *))
     (dumb-jump-handle-results results "src/file.js" "/code/redux" "" "isNow" nil))))


(ert-deftest dumb-jump-go-unsaved-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (buffer-modified-p *) => t)
       (mock (dumb-jump-message "Please save your file before jumping."))
       (dumb-jump-go)))))

(ert-deftest dumb-jump-go-nogrep-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (dumb-jump-ag-installed?) => nil)
       (mock (dumb-jump-grep-installed?) => nil)
       (mock (dumb-jump-message "Please install ag or grep!"))
       (dumb-jump-go)))))

(ert-deftest dumb-jump-go-nosymbol-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (with-mock
       (mock (dumb-jump-message "No symbol under point."))
       (dumb-jump-go)))))

(ert-deftest dumb-jump-message-get-results-unsaved-test ()
  (with-mock
   (mock (buffer-modified-p *) => t)
   (let ((results (dumb-jump-get-results)))
     (should (eq (plist-get results :issue) 'unsaved)))))

(ert-deftest dumb-jump-message-get-results-nogrep-test ()
  (with-mock
   (mock (dumb-jump-ag-installed?) => nil)
   (mock (dumb-jump-grep-installed?) => nil)
   (let ((results (dumb-jump-get-results)))
     (should (eq (plist-get results :issue) 'nogrep)))))

(ert-deftest dumb-jump-message-result-follow-test ()
  (with-mock
   (mock (dumb-jump-goto-file-line "src/file.js" 62 4))
   (let ((result '(:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
     (dumb-jump-result-follow result))))

(ert-deftest dumb-jump-message-result-follow-tooltip-test ()
  (with-mock
   (mock (popup-tip "/file.js:62 var isNow = true"))
   (let ((result '(:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
     (dumb-jump-result-follow result t "src"))))

(ert-deftest dumb-jump-populate-regexes-test ()
  (should (equal (dumb-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") nil) '("testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") nil) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumb-jump-message-prin1-test ()
  (with-mock
   (mock (message "%s %s" "(:path \"test\" :line 24)" "3"))
   (dumb-jump-message-prin1 "%s %s" '(:path "test" :line 24) 3)))

(ert-deftest dumb-jump-message-test ()
  (with-mock
   (mock (message "%s %s" "two" "three"))
   (dumb-jump-message "%s %s" "two" "three")))

(ert-deftest dumb-jump-concat-command-test ()
  (should (string= (dumb-jump-concat-command " test1 " "test2 " "   test3")
                   "test1 test2 test3")))

(ert-deftest dumb-jump-issue-result-test ()
  (let ((result (dumb-jump-issue-result "unsaved")))
    (should (eq (plist-get result :issue) 'unsaved))))

(ert-deftest dumb-jump-process-symbol-by-lang-test ()
  (let ((result (dumb-jump-process-symbol-by-lang "elisp" "somefunc"))
        (result2 (dumb-jump-process-symbol-by-lang "clojure" "myns/myfunc"))
        (result3 (dumb-jump-process-symbol-by-lang "ruby" ":myrubyfunc")))
    (should (string= result "somefunc"))
    (should (string= result2 "myfunc"))
    (should (string= result3 "myrubyfunc"))))

(ert-deftest dumb-jump-result-follow-test ()
  (let* ((data '(:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a")))
    (with-mock
     (mock (dumb-jump-goto-file-line "/usr/blah/test2.txt" 52 1))
     (dumb-jump-result-follow data nil "/usr/blah"))))

(ert-deftest dumb-jump-find-start-pos-test ()
  (let ((cur-pos 9)
        (line "event event")
        (word "event"))
    (should (= (dumb-jump-find-start-pos line word cur-pos) 6))))

(ert-deftest dumb-jump-go-include-lib-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el"))
        (lib-file (f-join test-data-dir-elisp "../fake-library/lib.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 23)
      (forward-char 3)
      (with-mock
       (mock (dumb-jump-goto-file-line * 4 7))
        (should (string= (dumb-jump-go) lib-file))))))

(ert-deftest dumb-jump-parse-response-line-test ()
  (let ((t1 (dumb-jump-parse-response-line "/opt/test/foo.js:44: var test = 12;" "/opt/test/blah.js"))
        (t2 (dumb-jump-parse-response-line "47: var test = 13;" "/opt/test/blah.js"))
        (t3 (dumb-jump-parse-response-line "c:\\Users\\test\\foo.js:1: var test = 14;" "c:\\Users\\test\\foo.js"))
        (t4 (dumb-jump-parse-response-line "c:\\Users\\test\\foo2.js:2:test = {a:1,b:1};" "c:\\Users\\test\\foo.js"))
        (t5 (dumb-jump-parse-response-line "/opt/test/foo1.js:41: var test = {c:3, d: 4};" "/opt/test/b2.js")))
    ;; normal
    (should (equal t1 '("/opt/test/foo.js" "44" " var test = 12;")))
    ;; no file name in response (searched single file)
    (should (equal t2 '("/opt/test/blah.js" "47" " var test = 13;")))
    ;; windows
    (should (equal t3 '("c:\\Users\\test\\foo.js" "1" " var test = 14;")))
    ;; windows w/ extra :
    (should (equal t4 '("c:\\Users\\test\\foo2.js" "2" "test = {a:1,b:1};")))
    ;; normal w/ extra :
    (should (equal t5 '("/opt/test/foo1.js" "41" " var test = {c:3, d: 4};")))))

;; react tests

(ert-deftest dumb-jump-react-test1 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 2)
      (with-mock
       (mock (dumb-jump-goto-file-line * 3 6))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-react-test2 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (forward-char 2)
      (with-mock
       (mock (dumb-jump-goto-file-line * 13 6))
       (should (string= js-file (dumb-jump-go)))))))


(ert-deftest dumb-jump-react-test3 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 27)
      (forward-char 2)
      (with-mock
       (mock (dumb-jump-goto-file-line * 26 6))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-react-test4 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 32)
      (forward-char 7)
      (with-mock
       (mock (dumb-jump-goto-file-line * 31 6))
       (should (string= js-file (dumb-jump-go)))))))

(ert-deftest dumb-jump-react-test5 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 39)
      (forward-char 2)
      (with-mock
       (mock (dumb-jump-goto-file-line * 37 6))
       (should (string= js-file (dumb-jump-go)))))))
