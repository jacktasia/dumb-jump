;;; -*- lexical-binding: t -*-
(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
;;; Code:

;; TODO: nuke me: this should be using (xref-backend-definitions 'dumber-jump nil)
(defun dumber-jump-go (&optional use-tooltip prefer-external prompt)
  (interactive "P")
  (let* ((start-time (float-time))
         (info (dumber-jump-get-results prompt))
         (end-time (float-time))
         (fetch-time (- end-time start-time))
         (results (plist-get info :results))
         (issue (plist-get info :issue))
         (lang (plist-get info :lang))
         (result-count (length results)))
    (cond
     ((eq issue 'nogrep)
      "Please install rg!")
     ((eq issue 'nosymbol)
      "No symbol under point.")
     ((s-ends-with? " file" lang)
      (dumber-jump-message "Could not find rules for '%s'." lang))
     ((= result-count 1)
      (car results))
     ((> result-count 1)
      (if dumber-jump-aggressive
          (car results)
        results))
     ((= result-count 0)
      '(:path nil :line nil)))))

(defun goto-line-and-col (l c)          ; TODO clean this crap up
  (goto-char (point-min))
  (forward-line l)
  (forward-char c))

(defun dumber-jump-should-go (path line)
  (let ((xref-backend-functions (list #'dumber-jump-xref-activate))
        (result (dumber-jump-go)))
    (should (equal path (plist-get result :path)))
    (should (equal line (plist-get result :line)))))

(defun dumber-jump-generators-by-searcher (searcher) ; TODO: remove
  "For a SEARCHER it yields a response parser, a command
generator function, an installed? function, and the corresponding
searcher symbol."
  `(:parse ,'dumber-jump-parse-rg-response
           :generate ,'dumber-jump-generate-rg-command
           :installed ,'dumber-jump-rg-installed?
           :searcher ,searcher))

(defun dumber-jump-pick-grep-variant (&optional proj-root) ; TODO: remove
  (dumber-jump-generators-by-searcher 'rg))

(defun dumber-jump-run-test (test cmd)
  "Use TEST as the standard input for the CMD."
  (with-temp-buffer
    (insert test)
    (shell-command-on-region (point-min) (point-max) cmd nil t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dumber-jump-test-rg-rules (&optional run-not-tests)
  "Test all the rg rules and return count of those that fail.
Optionally pass t for RUN-NOT-TESTS to see a list of all failed rules"
  (let ((fail-tmpl "rg FAILURE '%s' %s in response '%s' | CMD: '%s' | rule: '%s'"))
    (-mapcat
     (lambda (rule)
       (-mapcat
        (lambda (test)
          (let* ((cmd (concat "rg --color never --no-heading -U --pcre2 "
                              (shell-quote-argument (dumber-jump-populate-regex (plist-get rule :regex) "test" 'rg))))
                 (resp (dumber-jump-run-test test cmd)))
            (when (or
                   (and (not run-not-tests) (not (s-contains? test resp)))
                   (and run-not-tests (> (length resp) 0)))
              (list (format fail-tmpl test (if run-not-tests "IS unexpectedly" "NOT") resp cmd rule)))))
        (plist-get rule (if run-not-tests :not :tests))))
     (--filter (member "rg" (plist-get it :supports)) dumber-jump-find-rules))))

(defun dumber-jump-output-rule-test-failures (failures)
  (--each failures (princ (format "\t%s\n" it))))

(setq test-data-dir (f-expand "./test/data"))
(setq test-data-dir-elisp (f-join test-data-dir "proj2-elisp"))
(setq test-data-dir-proj1 (f-join test-data-dir "proj1"))
(setq test-data-dir-proj3 (f-join test-data-dir "proj3-clj"))
(setq test-data-dir-multiproj (f-join test-data-dir "multiproj"))

(ert-deftest data-dir-exists-test ()
  (should (f-dir? test-data-dir)))

(ert-deftest data-dir-proj2-exists-test ()
  (should (f-dir? test-data-dir-elisp)))

(ert-deftest dumber-jump-get-lang-by-ext-test ()
  (let ((lang1 (dumber-jump-get-language-by-filename "sldkfj.el"))
        (lang1b (dumber-jump-get-language-by-filename "sldkfj.el.gz"))
        (lang2 (dumber-jump-get-language-by-filename "/askdfjkl/somefile.js"))
        (nolang (dumber-jump-get-language-by-filename "/blah/somefile.bin")))
    (should (string= lang1 "elisp"))
    (should (string= lang1b "elisp"))
    (should (string= lang2 "javascript"))
    (should (null nolang))))

(ert-deftest dumber-jump-get-lang-major-mode-test ()
  (let* ((major-mode 'php)
         (lang1 (dumber-jump-get-language "blah/file.install"))
         (lang1b (dumber-jump-get-mode-base-name))
         (lang2 (dumber-jump-get-language-by-filename "/askdfjkl/somefile.js")))
    (should (string= lang1 "php"))
    (should (string= lang1b "php"))
    (should (string= lang2 "javascript"))))

(ert-deftest dumber-jump-current-files-results-test ()
  (let ((results '((:path "blah") (:path "rarr")))
        (expected '((:path "blah"))))
    (should (equal (dumber-jump-current-file-results "blah" results) expected))))

(ert-deftest dumber-jump-exclude-path-test ()
  (let* ((expected (list (f-join test-data-dir-proj1 "ignored")
                         (f-join test-data-dir-proj1 "ignored2")))
         (root (dumber-jump-get-project-root test-data-dir-proj1))
         (config (dumber-jump-read-config test-data-dir-proj1  ".dumbjump")))
    (should (equal (plist-get config :exclude)  expected))))

(ert-deftest dumber-jump-include-path-test ()
  (let* ((config (dumber-jump-read-config test-data-dir-proj1 ".dumbjump-include"))
         (expected (list (f-join test-data-dir-proj1 "../fake-library")
                         "/etc/var/some/code")))
    (should (equal (plist-get config :include) expected))))

(ert-deftest dumber-jump-exclude-path-blank-test ()
  (let* ((config (dumber-jump-read-config test-data-dir-proj1 ".dumbjump-blank")))
    (should (null (plist-get config :exclude)))
    (should (null (plist-get config :include)))))

(ert-deftest dumber-jump-config-lang-test ()
  (let* ((config (dumber-jump-read-config test-data-dir-proj1 ".dumbjump-lang")))
    (should (string= "python" (plist-get config :language)))))

(ert-deftest dumber-jump-generate-rg-command-no-ctx-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (expected (concat "rg --color never --no-heading --line-number -U --pcre2 --type elisp " (shell-quote-argument expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-rg-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-rg-command-remote-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (expected (concat "rg --color never --no-heading --line-number -U --pcre2 --type elisp -g \\!this/is/excluded " (shell-quote-argument expected-regexes) " /path/to/proj-root")))
    (should (string= expected  (dumber-jump-generate-rg-command  "tester" "blah.el" "/path/to/proj-root" regexes "elisp" '("/path/to/proj-root/this/is/excluded"))))))

(ert-deftest dumber-jump-generate-rg-command-no-ctx-extra-args ()
  ;; rg-args
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (dumber-jump-rg-search-args "--no-pcre2 --follow")
         (expected (concat "rg --color never --no-heading --line-number -U --no-pcre2 --follow --type elisp " (shell-quote-argument expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-rg-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-bad-rg-command-test ()
  (should (s-blank? (dumber-jump-generate-rg-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumber-jump-rg-parse-test ()
  (let* ((resp "./dumber-jump.el:22:(defun dumber-jump-asdf ()\n./dumber-jump.el:26:(defvar some-var )\n./dumber-jump2.el:28:1:(defvar some-var)")
         (parsed (dumber-jump-parse-rg-response resp "dumber-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumber-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumber-jump-run-cmd-test ()
  (let* ((gen-funcs (dumber-jump-pick-grep-variant test-data-dir-elisp))
         (parse-fn (plist-get gen-funcs :parse))
         (generate-fn (plist-get gen-funcs :generate))
         (searcher (plist-get gen-funcs :searcher))
         (regexes (dumber-jump-get-contextual-regexes "elisp" nil searcher))
         (results (dumber-jump-run-command "another-fake-function" test-data-dir-elisp regexes "" ""
                                         "blah.el" 3 parse-fn generate-fn))
        (first-result (car results)))
    (should (s-contains? "/fake.el" (plist-get first-result :path)))
    (should (= (plist-get first-result :line) 6))))

(ert-deftest dumber-jump-run-cmd-fail-test ()
  (let* ((gen-funcs (dumber-jump-pick-grep-variant test-data-dir-elisp))
         (parse-fn (plist-get gen-funcs :parse))
         (generate-fn (plist-get gen-funcs :generate))
         (results (dumber-jump-run-command "hidden-function" test-data-dir-elisp nil "" "" "blah.el" 3
                                         parse-fn generate-fn))
        (first-result (car results)))
    (should (null first-result))))

(ert-deftest dumber-jump-find-proj-root-test ()
  (let* ((js-file (f-join test-data-dir-proj1 "src" "js"))
         (found-project (dumber-jump-get-project-root js-file)))
    (should (f-exists? found-project))
    (should (string= found-project test-data-dir-proj1))
    (should (string= ".dumbjump" (dumber-jump-get-config found-project)))))

(ert-deftest dumber-jump-find-proj-root-default-test ()
  (with-mock (mock (locate-dominating-file * *))
    (let ((found-project (dumber-jump-get-project-root ""))
          (expected (f-expand dumber-jump-default-project)))
      (should (string= found-project expected)))))

(ert-deftest dumber-jump-get-point-symbol-region-active-test ()
  (with-mock
   (mock (region-active-p) => t)
   (mock (region-beginning) => 0)
   (mock (region-end) => 1)
   (mock (buffer-substring-no-properties * *) => "blah")
   (dumber-jump-get-point-symbol)))

(ert-deftest dumber-jump-test-rg-rules-test ()
  (let ((rule-failures (dumber-jump-test-rg-rules)))
    (dumber-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0))))

(when (dumber-jump-rg-installed?)
  (ert-deftest dumber-jump-test-rg-rules-not-test () ;; :not tests
    (let ((rule-failures (dumber-jump-test-rg-rules t)))
      (dumber-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(when (dumber-jump-rg-installed?)
  (ert-deftest dumber-jump-test-rg-rules-fail-test ()
    (let* ((bad-rule '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
           (dumber-jump-find-rules (cons bad-rule dumber-jump-find-rules))
           (rule-failures (dumber-jump-test-rg-rules)))
      (should (= (length rule-failures) 1)))))

(ert-deftest dumber-jump-match-test ()
  (should (not (dumber-jump-re-match nil "asdf")))
  (should (dumber-jump-re-match "^asdf$" "asdf"))
  (should (string= (car (dumber-jump-re-match "^[0-9]+$" "123")) "123")))

(ert-deftest dumber-jump-context-point-test ()
  (let* ((sentence "mainWindow.loadUrl('file://')")
         (func "loadUrl")
         (ctx (dumber-jump-get-point-context sentence func 11)))
         (should (string= (plist-get ctx :left) "mainWindow."))
         (should (string= (plist-get ctx :right) "('file://')"))))

(ert-deftest dumber-jump-context-point-type-test ()
  (let* ((sentence "mainWindow.loadUrl('file://' + __dirname + '/dt/inspector.html?electron=true');")
         (func "loadUrl")
         (pt-ctx (dumber-jump-get-point-context sentence func 11))
         (ctx-type (dumber-jump-get-ctx-type-by-language "javascript" pt-ctx)))
    (should (string= ctx-type "function"))))

(ert-deftest dumber-jump-fetch-results-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 10)
      (let ((results (dumber-jump-fetch-file-results)))
        (should (string= "doSomeStuff" (plist-get results :symbol)))
        (should (string= "javascript" (plist-get results :lang)))))))

(ert-deftest dumber-jump-go-shell-test ()
  (let* ((go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js"))
         (default-directory test-data-dir-proj1))
    (with-current-buffer (get-buffer-create "*shell*")
      (insert ".js doSomeStuff()")
      (goto-char (point-min))
      (forward-char 6)
      (let ((results (dumber-jump-get-results)))
        (should (string= "doSomeStuff" (plist-get results :symbol)))
        (should (string= "javascript" (plist-get results :lang)))))))

(ert-deftest dumber-jump-go-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (dumber-jump-should-go go-js-file 3))))

(ert-deftest dumber-jump-go-current-window-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (dumber-jump-should-go go-js-file 3))))

(ert-deftest dumber-jump-go-js2-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 11)
      (forward-char 76)
      (dumber-jump-should-go js-file 7))))

(ert-deftest dumber-jump-go-js-es6a-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (dumber-jump-should-go js-file 1))))

(ert-deftest dumber-jump-go-js-es6b-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 21)
      (dumber-jump-should-go js-file 3))))

(ert-deftest dumber-jump-go-js-es6c-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (dumber-jump-should-go js-file 5))))

(ert-deftest dumber-jump-go-js-es6d-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 23)
      (dumber-jump-should-go js-file 10))))

(ert-deftest dumber-jump-go-js-es6e-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 24)
      (dumber-jump-should-go js-file 16))))

(ert-deftest dumber-jump-go-js-es6-class-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 36)
      (forward-char 12)
      (dumber-jump-should-go js-file 28))))

(ert-deftest dumber-jump-go-sig-def-test () :expected-result :failed
  (let ((dumber-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 7)
      (forward-char 35)
      (dumber-jump-should-go js-file 6))))

(ert-deftest dumber-jump-go-sig-def2-test () :expected-result :failed
  (let ((dumber-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 35)
      (dumber-jump-should-go js-file 12))))

(ert-deftest dumber-jump-go-sig-def3-test () :expected-result :failed
  (let ((dumber-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (forward-char 35)
      (dumber-jump-should-go js-file 19))))

(ert-deftest dumber-jump-go-var-let-test () :expected-result :failed
  (let ((dumber-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 33)
      (dumber-jump-should-go el-file 11))))

(ert-deftest dumber-jump-go-var-let-repeat-test () :expected-result :failed
  (let ((dumber-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 21)
      (forward-char 33)
      (dumber-jump-should-go el-file 18))))

(ert-deftest dumber-jump-go-var-arg-test ()
  (let ((dumber-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 12)
      (dumber-jump-should-go el-file 3))))

(ert-deftest dumber-jump-go-no-result-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 4)
      (dumber-jump-should-go nil nil))))

(ert-deftest dumber-jump-go-no-rules-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt")))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (with-mock
       (mock (dumber-jump-message "Could not find rules for '%s'." ".txt file"))
       (dumber-jump-go)))))

(ert-deftest dumber-jump-rg-installed?-test-no ()
  (let ((dumber-jump--rg-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "ripgrep 0.3.1\n" :times 1)
     (should (not (eq (dumber-jump-rg-installed?) t)))
     ;; confirm memoization of the previous result
     (should (not (eq (dumber-jump-rg-installed?) t))))))

(ert-deftest dumber-jump-rg-installed?-test-yes ()
  (let ((dumber-jump--rg-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "ripgrep 0.10.0\n" :times 1)
     (should (eq (dumber-jump-rg-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-rg-installed?) t)))))

(ert-deftest dumber-jump-rg-installed?-test-yes2 ()
  (let ((dumber-jump--rg-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "ripgrep 1.1.0\n" :times 1)
     (should (eq (dumber-jump-rg-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-rg-installed?) t)))))

(ert-deftest dumber-jump-go-nogrep-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)

      (with-mock
       (mock (dumber-jump-rg-installed?) => nil)
       (should (equal "Please install rg!" (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-nosymbol-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (should (equal "No symbol under point." (dumber-jump-go))))))

(ert-deftest dumber-jump-message-get-results-nogrep-test ()
  (with-mock
   (mock (dumber-jump-rg-installed?) => nil)
   (let ((results (dumber-jump-get-results)))
     (should (eq (plist-get results :issue) 'nogrep)))))

(ert-deftest dumber-jump-populate-regexes-rg-test ()
  (should (equal (dumber-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'rg) '("testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'rg) '("\\$testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "-testvar" '("JJJ\\s*=\\s*") 'rg) '("[-]testvar\\s*=\\s*"))))

(ert-deftest dumber-jump-message-test ()
  (with-mock
   (mock (message "%s %s" "two" "three"))
   (dumber-jump-message "%s %s" "two" "three")))

(ert-deftest dumber-jump-concat-command-test ()
  (should (string= (dumber-jump-concat-command " test1 " "test2 " "   test3")
                   "test1 test2 test3")))

(ert-deftest dumber-jump-issue-result-test ()
  (let ((result (dumber-jump-issue-result "unsaved")))
    (should (eq (plist-get result :issue) 'unsaved))))

(ert-deftest dumber-jump-process-symbol-by-lang-test ()
  (let ((result (dumber-jump-process-symbol-by-lang "elisp" "somefunc"))
        (result2 (dumber-jump-process-symbol-by-lang "clojure" "myns/myfunc"))
        (result3 (dumber-jump-process-symbol-by-lang "ruby" ":myrubyfunc"))
        (result3b (dumber-jump-process-symbol-by-lang "ruby" "Health::Checks::QueueGrowth"))
        (result3c (dumber-jump-process-symbol-by-lang "ruby" "::Health"))
        (result4 (dumber-jump-process-symbol-by-lang "systemverilog" "`myvlfunc")))
    (should (string= result "somefunc"))
    (should (string= result2 "myfunc"))
    (should (string= result3 "myrubyfunc"))
    (should (string= result3b "QueueGrowth"))
    (should (string= result3c "Health"))
    (should (string= result4 "myvlfunc"))))

(ert-deftest dumber-jump-go-include-lib-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el"))
        (lib-file (f-join test-data-dir-elisp "../fake-library/lib.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 23)
      (forward-char 3)
      (dumber-jump-should-go lib-file 4))))

(ert-deftest dumber-jump-parse-response-line-test ()
  (let ((t1 (dumber-jump-parse-response-line "/opt/test/foo.js:44: var test = 12;" "/opt/test/blah.js"))
        (t1b (dumber-jump-parse-response-line "/path/to/file.f90:1701: subroutine test(foo)" "/path/to/file2.f90"))
        (t2 (dumber-jump-parse-response-line "47: var test = 13;" "/opt/test/blah.js"))
        (t3 (dumber-jump-parse-response-line "c:\\Users\\test\\foo.js:1: var test = 14;" "c:\\Users\\test\\foo.js"))
        (t4 (dumber-jump-parse-response-line "c:\\Users\\test\\foo2.js:2:test = {a:1,b:1};" "c:\\Users\\test\\foo.js"))
        (t5 (dumber-jump-parse-response-line "/opt/test/foo1.js:41: var test = {c:3, d: 4};" "/opt/test/b2.js")))
    ;; normal
    (should (equal t1 '("/opt/test/foo.js" "44" " var test = 12;")))
    ;; normal fortran
    (should (equal t1b '("/path/to/file.f90" "1701"  " subroutine test(foo)")))
    ;; no file name in response (searched single file)
    (should (equal t2 '("/opt/test/blah.js" "47" " var test = 13;")))
    ;; windows
    (should (equal t3 '("c:\\Users\\test\\foo.js" "1" " var test = 14;")))
    ;; windows w/ extra :
    (should (equal t4 '("c:\\Users\\test\\foo2.js" "2" "test = {a:1,b:1};")))
    ;; normal w/ extra :
    (should (equal t5 '("/opt/test/foo1.js" "41" " var test = {c:3, d: 4};")))))

(ert-deftest dumber-jump-rgtype-test ()
  (should (equal (dumber-jump-get-rg-type-by-language "python") '("py"))))

;; react tests

(ert-deftest dumber-jump-react-test1 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 2)
      (dumber-jump-should-go js-file 3))))

(ert-deftest dumber-jump-react-test2 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (forward-char 2)
      (dumber-jump-should-go js-file 13))))

(ert-deftest dumber-jump-react-test3 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 27)
      (forward-char 2)
      (dumber-jump-should-go js-file 26))))

(ert-deftest dumber-jump-react-test4 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 32)
      (forward-char 7)
      (dumber-jump-should-go js-file 31))))

(ert-deftest dumber-jump-react-test5 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 39)
      (forward-char 2)
      (dumber-jump-should-go js-file 37))))

;; c++ tests

(ert-deftest dumber-jump-cpp-test1 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "test.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 14)
      (dumber-jump-should-go cpp-file 3))))

(ert-deftest dumber-jump-cpp-test2 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "test.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 9)
      (dumber-jump-should-go cpp-file 1))))

(ert-deftest dumber-jump-cpp-issue87 () :expected-result :failed
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "issue-87.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 16)
      (forward-char 12)
      (dumber-jump-should-go cpp-file 6))))

;; This test verifies that having ".dumbjumpignore" files in the two sub-projects will make it find
;; the "multiproj" folder as project root since it has a ".dumbjump" file. The two sub-projects have
;; a dummy ".git" folder to signify it as a repository that would normally become the root without
;; the ignore file.
(ert-deftest dumber-jump-multiproj ()
  (let ((main-file (f-join test-data-dir-multiproj "subproj1" "main.cc"))
        (header-file (f-join test-data-dir-multiproj "subproj2" "header.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 18)
      (dumber-jump-should-go header-file 6))))

(ert-deftest dumber-jump-filter-no-start-comments ()
  (should (equal '((:context "yield me"))
                 (dumber-jump-filter-no-start-comments '((:context "// filter me out")
                                                       (:context "yield me")) "c++"))))

(ert-deftest dumber-jump-filter-no-start-comments-unknown-language ()
  (should (equal nil (dumber-jump-filter-no-start-comments '() "unknownlanguage"))))

(defun generators-valid (pl searcher)
  (and (eq 8 (length pl))
       (functionp (plist-get pl :parse))
       (functionp (plist-get pl :generate))
       (functionp (plist-get pl :installed))
       (eq searcher (plist-get pl :searcher))))

(ert-deftest dumber-jump-generators-by-searcher-rg ()
  (let* ((searcher 'rg)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

;; Make sure it jumps aggressively, i.e. normally.
(ert-deftest dumber-jump-handle-results-aggressively-test () :expected-result :failed
  (let ((dumber-jump-aggressive t)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-goto-file-line "relfile.js" 62 4))
     (dumber-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

;; Make sure non-aggressive mode shows choices when more than one possibility.
(ert-deftest dumber-jump-handle-results-non-aggressively-test () :expected-result :failed
  (let ((dumber-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-prompt-user-for-choice "/code/redux" *))
     (dumber-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

;; Make sure it jumps when there's only one possibility in non-aggressive mode.
(ert-deftest dumber-jump-handle-results-non-aggressive-do-jump-test () :expected-result :failed
  (let ((dumber-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-goto-file-line "relfile.js" 62 4))
     (dumber-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumber-jump-shell-command-switch-zsh-test ()
  (let ((shell-file-name "/usr/bin/zsh"))
    (should (string-equal "-icf" (dumber-jump-shell-command-switch)))))

(ert-deftest dumber-jump-shell-command-switch-csh-test ()
  (let ((shell-file-name "/usr/bin/csh"))
    (should (string-equal "-icf" (dumber-jump-shell-command-switch)))))

(ert-deftest dumber-jump-shell-command-switch-tcsh-test ()
  (let ((shell-file-name "/usr/bin/zsh"))
    (should (string-equal "-icf" (dumber-jump-shell-command-switch)))))

(ert-deftest dumber-jump-shell-command-switch-bash-test ()
  (let ((shell-file-name "/usr/bin/bash"))
    (should (string-equal "-c" (dumber-jump-shell-command-switch)))))

(ert-deftest dumber-jump-shell-command-switch-unknown-test ()
  (let ((shell-file-name "/usr/bin/thisshelldoesnotexist"))
    (should (string-equal shell-command-switch (dumber-jump-shell-command-switch)))))

(ert-deftest dumber-jump-go-clojure-question-mark-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file2.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode)  ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 2)
      (dumber-jump-should-go clj-to-file 1))))

(ert-deftest dumber-jump-go-clojure-no-question-mark-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file1.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 2)
      (dumber-jump-should-go clj-to-file 2))))

(ert-deftest dumber-jump-go-clojure-no-asterisk-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file2.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 2)
      (dumber-jump-should-go clj-to-file 4))))

(ert-deftest dumber-jump-go-clojure-asterisk-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file1.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 5)
      (forward-char 2)
      (dumber-jump-should-go clj-to-file 5))))

;;;
