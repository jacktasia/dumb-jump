;;; -*- lexical-binding: t -*-
(require 'f)
(require 's)
(require 'dash)
(require 'noflet)
(require 'el-mock)
(require 'popup)
;;
(require 'dumb-jump)
;;; Code:

(defun dumb-jump-output-rule-test-failures (failures)
  (--each failures (princ (format "\t%s\n" it))))

;; The test data directory path is adjusted to the location of the
;; current working directory being either inside the top directory
;; where dumb-jump.el and test directory are located or inside the test
;; sub-directory where the test file and the data sub-directory are
;; located. Anywhere else is invalid.
(defconst test-data-dir (if (and (f-exists? "./dumb-jump.el")
                                 (f-dir? "./test/data"))
                            (f-expand "./test/data")
                          (if (and (f-exists? "./dumb-jump-test.el")
                                   (f-dir? "./data"))
                              (f-expand "./data")
                            "INVALID-Current-Working-Directory-for-ERT-Tests!"))
  "Test directory.")
(defconst test-dumb-jump-root-dir (if (and (f-exists? "./dumb-jump.el")
                                           (f-dir? "./test/data"))
                                      (f-expand ".")
                          (if (and (f-exists? "./dumb-jump-test.el")
                                   (f-dir? "./data"))
                              (f-expand "..")
                            "INVALID-Current-Working-Directory-for-ERT-Tests!"))
  "Test directory.")

(defconst test-data-dir-elisp (f-join test-data-dir "proj2-elisp")
  "Test directory.")

(defconst test-data-dir-proj1 (f-join test-data-dir "proj1")
  "Test directory.")
(defconst test-data-dir-proj3 (f-join test-data-dir "proj3-clj")
  "Test directory.")
(defconst test-data-dir-multiproj (f-join test-data-dir "multiproj")
  "Test directory.")

(defconst test-dumb-jump-file-path (if (and (f-exists? "./dumb-jump.el")
                                            (f-dir? "./test/data"))
                                       "dumb-jump.el"
                                     (if (and (f-exists? "./dumb-jump-test.el")
                                              (f-dir? "./data"))
                                         "../dumb-jump.el"
                                       "INVALID-Current-Working-Directory-for-dumb-jump!"))
  "Relative path of dumb-jump.el ")

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
         (lang1b (dumb-jump-get-mode-base-name))
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
        (expected " --include \\*.js --include \\*.jsx --include \\*.vue --include \\*.html --include \\*.css "))
    (should (string= expected args))))

(defconst dump-jump--expected-elisp-regexps-templates
  '("\\((defun|cl-defun|cl-defgeneric|cl-defmethod|cl-defsubst)\\s+JJJ\\j"
    "\\((defmacro|cl-defmacro|cl-define-compiler-macro)\\s+JJJ\\j"
    "\\(defhydra\\b\\s*JJJ\\j"
    ;;
    "\\(defvar(-local)?\\b\\s*JJJ\\j"
    "\\(defconst\\b\\s*JJJ\\j"
    "\\(defcustom\\b\\s*JJJ\\j"
    "\\(setq\\b\\s*JJJ\\j"
    "\\(JJJ\\s+"
    "\\((cl-defstruct|cl-deftype)\\s+JJJ\\j"
    "\\((defun|cl-defun|cl-defgeneric|cl-defmethod)\\s*.+\\(?\\s*JJJ\\j\\s*\\)?")
  "List of regexp templates equal to what is in dumb-jump.el.")

(defun dumb-jump--elisp-expected-regexps (variant &optional type)
  "Return a list of elisp regexps adjusted with word boundary of VARIANT.
VARIANT must be one of: ag, rg, grep, gnu-grep, git-grep, or git-grep-plus-ag."
  (let ((regexes (if (eq type 'elisp-functions)
                     ;; take the first 3 regexps: used for functions
                     (-take 3 dump-jump--expected-elisp-regexps-templates)
                   dump-jump--expected-elisp-regexps-templates))
        (word-boundary-regexp
         (symbol-value
          (intern
           (format "dumb-jump-%s-word-boundary"
                   (if (eq variant 'git-grep-plus-ag)
                       'ag
                     variant)))))
        (formatted-regexps '()))
    ;; perform translation of each regexp template
    (dolist (r regexes (reverse formatted-regexps))
      (setq r (s-replace "JJJ" "tester" r))
      (setq r (s-replace "\\j" word-boundary-regexp r))
      (when (dumb-jump-use-space-bracket-exp-for variant)
        (setq r (s-replace "\\s" "[[:space:]]" r)))
      (push r formatted-regexps))))

(ert-deftest dumb-jump-generate-grep-command-no-ctx-test ()
  (let* ((system-type 'darwin)
         (regexes (dumb-jump-get-contextual-regexes "elisp" nil 'grep))
         ;; The grep command built by dumb-jump places each regexp inside its
         ;; own -e string.  Each of those strings following -e must be
         ;; shell-quoted.
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  (dumb-jump--elisp-expected-regexps 'grep)))

         (expected (concat
                    "LANG=C grep -REn --include \\*.el --include \\*.el.gz"
                    (s-join "" expected-regexes)
                    " .")))
    (should (string= expected  (dumb-jump-generate-grep-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-gnu-grep-command-no-ctx-test ()
  (let* ((system-type 'darwin)
         (regexes (dumb-jump-get-contextual-regexes "elisp" nil 'gnu-grep))
         ;; The gnu grep command built by dumb-jump places each regexp inside
         ;; its own -e string. Each of those strings following -e must be
         ;; shell-quoted.
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  (dumb-jump--elisp-expected-regexps 'gnu-grep)))
         (expected (concat "LANG=C grep -rEn" (s-join "" expected-regexes) " .")))
    (should (string= expected  (dumb-jump-generate-gnu-grep-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-ag-command-no-ctx-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'ag))
         (base-regexes (dumb-jump--elisp-expected-regexps 'ag))
         ;; The ag tool accepts a list of regexps separated by "\\|".
         ;; Each of the regexp must be shell-quoted (but not "\\|").
         (expected-regexes
          (append (-map (lambda (elem)
                          (concat (shell-quote-argument elem) "\\|"))
                        (-butlast base-regexes))
                  (list (shell-quote-argument (-last-item base-regexes)))))
         (expected (concat "ag --nocolor --nogroup --elisp -- "
                           (mapconcat #'identity expected-regexes "")
                           " .")))
    (should (string= expected
                     (dumb-jump-generate-ag-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-ag-command-exclude-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes (mapconcat #'identity
                                      (dumb-jump--elisp-expected-regexps 'ag)
                                      "|"))
         (expected
          (concat
           "ag --nocolor --nogroup --elisp --ignore-dir this/is/excluded -- "
           (shell-quote-argument expected-regexes)
           " /path/to/proj-root")))
    (should (string= expected
                     (dumb-jump-generate-ag-command
                      "tester" "blah.el" "/path/to/proj-root"
                      regexes "elisp"
                      '("/path/to/proj-root/this/is/excluded"))))))

(ert-deftest dumb-jump-generate-git-grep-plus-ag-command-no-ctx-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes (mapconcat #'identity
                                      (dumb-jump--elisp-expected-regexps 'git-grep-plus-ag)
                                      "|"))
         (expected
          ;; NOTE no "--elisp" and the `-G` arg is new
          (concat "ag --nocolor --nogroup -G '(/path/to/proj-root/blah.el)' "
                  (shell-quote-argument expected-regexes)
                  " /path/to/proj-root")))
  (with-mock
   (mock
    (dumb-jump-get-git-grep-files-matching-symbol-as-ag-arg * *) => "'(/path/to/proj-root/blah.el)'")
    (should (string= expected
                     (dumb-jump-generate-git-grep-plus-ag-command
                      "tester" "blah.el" "/path/to/proj-root"
                      regexes "elisp" nil))))))


(ert-deftest dumb-jump-generate-git-grep-plus-ag-command-exclude-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes
          (mapconcat #'identity
                     (dumb-jump--elisp-expected-regexps 'git-grep-plus-ag)
                     "|"))
         (expected
          ;; NOTE no "--elisp" and the `-G` arg is new
          (concat "ag --nocolor --nogroup -G '(/path/to/proj-root/blah.el)' --ignore-dir this/is/excluded "
                  (shell-quote-argument expected-regexes)
                  " /path/to/proj-root")))
  (with-mock
   (mock (dumb-jump-get-git-grep-files-matching-symbol-as-ag-arg * *) => "'(/path/to/proj-root/blah.el)'")
    (should (string= expected
                     (dumb-jump-generate-git-grep-plus-ag-command
                      "tester" "blah.el" "/path/to/proj-root" regexes "elisp"
                      '("/path/to/proj-root/this/is/excluded")))))))


(ert-deftest dumb-jump-generate-rg-command-no-ctx-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes
          (mapconcat #'identity
                     (dumb-jump--elisp-expected-regexps 'rg)
                     "|"))
         (expected (concat "rg --color never --no-heading --line-number -U --pcre2 --type elisp -- "
                           (shell-quote-argument expected-regexes) " .")))
    (should (string= expected  (dumb-jump-generate-rg-command
                                "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-rg-command-remote-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes
          (mapconcat #'identity
                     (dumb-jump--elisp-expected-regexps 'rg)
                     "|"))
         (expected
          (concat "rg --color never --no-heading --line-number -U --pcre2 --type elisp -g \\!this/is/excluded -- "
                  (shell-quote-argument expected-regexes)
                  " /path/to/proj-root")))
    (should (string= expected
                     (dumb-jump-generate-rg-command
                      "tester" "blah.el" "/path/to/proj-root" regexes "elisp"
                      '("/path/to/proj-root/this/is/excluded"))))))

(ert-deftest dumb-jump-generate-git-grep-command-no-ctx-test ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'git-grep))
         (expected-regexes
          (mapconcat #'identity
                     (dumb-jump--elisp-expected-regexps 'git-grep)
                     "|"))
         (excludes '("one" "two" "three"))
         (expected
          (concat "git grep --color=never --line-number --untracked -E "
                  (shell-quote-argument expected-regexes)
                  " -- ./\\*.el ./\\*.el.gz \\:\\(exclude\\)one \\:\\(exclude\\)two \\:\\(exclude\\)three")))
    (should (string= expected
                     (dumb-jump-generate-git-grep-command
                      "tester" "blah.el" "." regexes "elisp" excludes)))))

(ert-deftest dumb-jump-generate-git-grep-command-no-ctx-extra-args ()
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'git-grep))
         (expected-regexes
          (mapconcat #'identity
                     (dumb-jump--elisp-expected-regexps 'git-grep)
                     "|"))
         (excludes '("one" "two" "three"))
         (dumb-jump-git-grep-search-args "--recurse-submodules")
         (expected
          (concat
           "git grep --color=never --line-number --untracked --recurse-submodules -E "
           (shell-quote-argument expected-regexes)
           " -- ./\\*.el ./\\*.el.gz \\:\\(exclude\\)one \\:\\(exclude\\)two \\:\\(exclude\\)three")))
    (should (string= expected
                     (dumb-jump-generate-git-grep-command
                      "tester" "blah.el" "." regexes "elisp" excludes)))))

(ert-deftest dumb-jump-generate-ag-command-no-ctx-extra-args ()
  ;; ag args
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes
          (mapconcat #'identity
                     (dumb-jump--elisp-expected-regexps 'ag)
                     "|"))
         (dumb-jump-ag-search-args "--follow")
         (expected
          (concat "ag --nocolor --nogroup --follow --elisp -- "
                  (shell-quote-argument expected-regexes) " .")))
    (should (string= expected
                     (dumb-jump-generate-ag-command
                      "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-rg-command-no-ctx-extra-args ()
  ;; rg-args
  (let* ((regexes (dumb-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes (mapconcat #'identity
                                      (dumb-jump--elisp-expected-regexps 'rg)
                                      "|"))
         (dumb-jump-rg-search-args "--no-pcre2 --follow")
         (expected
          (concat "rg --color never --no-heading --line-number -U --no-pcre2 --follow --type elisp -- "
                  (shell-quote-argument expected-regexes)
                  " .")))
    (should (string= expected
                     (dumb-jump-generate-rg-command
                      "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumb-jump-generate-git-grep-command-not-search-untracked-test ()
  (let* ((dumb-jump-git-grep-search-args "")
         (dumb-jump-git-grep-search-untracked nil)
         (regexes (dumb-jump-get-contextual-regexes "elisp" nil 'git-grep))
         (expected-regexes (mapconcat #'identity
                                      (dumb-jump--elisp-expected-regexps 'git-grep)
                                      "|"))
         (excludes '("one" "two" "three"))
         (expected
          (concat "git grep --color=never --line-number -E "
                  (shell-quote-argument expected-regexes)
                  " -- ./\\*.el ./\\*.el.gz \\:\\(exclude\\)one \\:\\(exclude\\)two \\:\\(exclude\\)three")))
    (should (string= expected
                     (dumb-jump-generate-git-grep-command
                      "tester" "blah.el" "." regexes "elisp" excludes)))))

(ert-deftest dumb-jump-generate-grep-command-no-ctx-funcs-only-test ()
  (let* ((system-type 'darwin)
         (dumb-jump-functions-only t)
         (regexes (dumb-jump-get-contextual-regexes "elisp" nil 'grep))
         (expected-regexes (s-join ""
                            (--map (concat " -e " (shell-quote-argument it))
                                   (dumb-jump--elisp-expected-regexps 'grep 'elisp-functions))))
         (expected (concat "LANG=C grep -REn" expected-regexes " ."))
         (zexpected (concat "LANG=C zgrep -REn" expected-regexes " .")))
    (should (string= expected  (dumb-jump-generate-grep-command  "tester" "blah.el" "." regexes "" nil)))
    (should (string= zexpected  (dumb-jump-generate-grep-command  "tester" "blah.el.gz" "." regexes "" nil)))))

(ert-deftest dumb-jump-generate-grep-command-with-ctx-test ()
  (let* ((system-type 'darwin)
         (ctx-type (dumb-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
         (dumb-jump-ignore-context nil) ;; overriding the default
         (regexes (dumb-jump-get-contextual-regexes "elisp" ctx-type 'grep))
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  (dumb-jump--elisp-expected-regexps 'grep 'elisp-functions)))
         (expected (concat "LANG=C grep -REn" (s-join "" expected-regexes) " .")))
    ;; the point context being passed should match a "function" type so only the one command
    (should (string= expected  (dumb-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil)))))

(ert-deftest dumb-jump-generate-grep-command-on-windows-test ()
  (noflet ((shell-quote-argument (it) (format "'%s'" it)))
    (let* ((system-type 'windows-nt)
           (ctx-type (dumb-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
           (dumb-jump-ignore-context nil) ;; overriding the default
           (regexes (dumb-jump-get-contextual-regexes "elisp" ctx-type 'grep))
           (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                    (dumb-jump--elisp-expected-regexps 'grep 'elisp-functions)))
           (expected (concat "grep -REn" (s-join "" expected-regexes) " .")))
      (should (string= expected  (dumb-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil))))))

(ert-deftest dumb-jump-generate-grep-command-with-ctx-but-ignored-test ()
  (let* ((system-type 'darwin)
         (ctx-type (dumb-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
         (dumb-jump-ignore-context t)
         (regexes (dumb-jump-get-contextual-regexes "elisp" ctx-type nil))
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  (dumb-jump--elisp-expected-regexps 'grep)))
         (expected (concat "LANG=C grep -REn" (s-join "" expected-regexes) " .")))

    ;; the point context being passed is ignored so ALL should return
    (should (string= expected  (dumb-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil)))))

(ert-deftest dumb-jump-generate-bad-grep-command-test ()
    (should (s-blank? (dumb-jump-generate-grep-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumb-jump-generate-bad-ag-command-test ()
    (should (s-blank? (dumb-jump-generate-ag-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumb-jump-generate-bad-rg-command-test ()
  (should (s-blank? (dumb-jump-generate-rg-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumb-jump-generate-bad-git-grep-command-test ()
    (should (s-blank? (dumb-jump-generate-git-grep-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

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

(ert-deftest dumb-jump-rg-parse-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar some-var )\n./dumb-jump2.el:28:1:(defvar some-var)")
         (parsed (dumb-jump-parse-rg-response resp "dumb-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumb-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumb-jump-git-grep-parse-test ()
  (let* ((resp "./dumb-jump.el:22:(defun dumb-jump-asdf ()\n./dumb-jump.el:26:(defvar some-var )\n./dumb-jump2.el:28:1:(defvar some-var)")
         (parsed (dumb-jump-parse-git-grep-response resp "dumb-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumb-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumb-jump-run-cmd-test ()
  (with-mock
    (stub dumb-jump-rg-installed? => t)
    (let* ((gen-funcs (dumb-jump-pick-grep-variant test-data-dir-elisp))
           (parse-fn (plist-get gen-funcs :parse))
           (generate-fn (plist-get gen-funcs :generate))
           (searcher (plist-get gen-funcs :searcher))
           (regexes (dumb-jump-get-contextual-regexes "elisp" nil searcher))
           (results (dumb-jump-run-command "another-fake-function" test-data-dir-elisp regexes "" ""
                                           "blah.el" 3 parse-fn generate-fn))
           (first-result (car results)))
      (should (s-contains? "/fake.el" (plist-get first-result :path)))
      (should (= (plist-get first-result :line) 6)))))

(ert-deftest dumb-jump-run-cmd-fail-test ()
  (with-mock
    (stub dumb-jump-rg-installed? => t)
    (let* ((gen-funcs (dumb-jump-pick-grep-variant test-data-dir-elisp))
           (parse-fn (plist-get gen-funcs :parse))
           (generate-fn (plist-get gen-funcs :generate))
           (results (dumb-jump-run-command "hidden-function" test-data-dir-elisp nil "" "" "blah.el" 3
                                           parse-fn generate-fn))
           (first-result (car results)))
      (should (null first-result)))))

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

(ert-deftest dumb-jump-get-point-symbol-region-active-test ()
  (with-mock
   (mock (region-active-p) => t)
   (mock (region-beginning) => 0)
   (mock (region-end) => 1)
   (mock (buffer-substring-no-properties * *) => "blah")
   (dumb-jump-get-point-symbol)))

(ert-deftest dumb-jump-goto-file-line-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-mock
     (when (version< emacs-version "29")
       (mock (ring-insert * *)))
     (dumb-jump-goto-file-line js-file 3 0)
     (should (string= (buffer-file-name) js-file))
     (should (= (line-number-at-pos) 3)))))

(ert-deftest dumb-jump-test-grep-rules-test ()
  (let ((rule-failures (dumb-jump-test-grep-rules)))
    (dumb-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0))))

(defun dumb-jump--noop (&rest _args)
  "Do nothing, return nil."
  nil)

(when (dumb-jump-ag-installed?)
  (ert-deftest dumb-jump-test-ag-rules-test ()
    ;; Some of the tests write text inside a temporary buffer and save the
    ;; text to a file to perform testing. That will work as long as Emacs is
    ;; not setup to execute `delete-trailing-whitespace' in the
    ;; `before-save-hook' which might be the case for users systems.
    ;; To prevent that from happening advice the `delete-trailing-whitespace'
    ;; function into something that does nothing for the duration of the test.
    (unwind-protect
        (progn
          (advice-add 'delete-trailing-whitespace :override 'dumb-jump--noop)
          (let ((rule-failures (dumb-jump-test-ag-rules)))
            (dumb-jump-output-rule-test-failures rule-failures)
            (should (= (length rule-failures) 0))))
      ;; restore `delete-trailing-whitespace'
      (advice-remove 'delete-trailing-whitespace 'dumb-jump--noop))))

(when (dumb-jump-rg-installed?)
  (ert-deftest dumb-jump-test-rg-rules-test ()
    (let ((rule-failures (dumb-jump-test-rg-rules)))
      (dumb-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(when (and (not (eq system-type 'darwin))   ; git grep is broken on macOS as of Jan 2026.
           (dumb-jump-git-grep-installed?))
  (ert-deftest dumb-jump-test-git-grep-rules-test ()
    (let ((rule-failures (dumb-jump-test-git-grep-rules)))
      (dumb-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(ert-deftest dumb-jump-test-grep-rules-not-test () ;; :not tests
  (let ((rule-failures (dumb-jump-test-grep-rules t)))
    (dumb-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0))))

(when (dumb-jump-ag-installed?)
  (ert-deftest dumb-jump-test-ag-rules-not-test () ;; :not tests
    (let ((rule-failures (dumb-jump-test-ag-rules t)))
    (dumb-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0)))))

(when (dumb-jump-rg-installed?)
  (ert-deftest dumb-jump-test-rg-rules-not-test () ;; :not tests
    (let ((rule-failures (dumb-jump-test-rg-rules t)))
      (dumb-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(ert-deftest dumb-jump-test-grep-rules-fail-test ()
  (let* ((bad-rule '(:type "variable"
                           :supports ("ag" "grep" "rg" "git-grep")
                           :language "elisp"
                           :regex "\\\(defvarJJJ\\b\\s*"
                           :tests ("(defvar test ")))
         (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
         (rule-failures (dumb-jump-test-grep-rules)))
    (should (= (length rule-failures) 1))))

(when (dumb-jump-ag-installed?)
  (ert-deftest dumb-jump-test-ag-rules-fail-test ()
    ;; Some of the tests write text inside a temporary buffer and save the
    ;; text to a file to perform testing. That will work as long as Emacs is
    ;; not setup to execute `delete-trailing-whitespace' in the
    ;; `before-save-hook' which might be the case for users systems.
    ;; To prevent that from happening advice the `delete-trailing-whitespace'
    ;; function into something that does nothing for the duration of the test.
    (unwind-protect
        (progn
          (advice-add 'delete-trailing-whitespace :override 'dumb-jump--noop)
          (let* ((bad-rule '(:type "variable"
                                   :supports ("ag" "grep" "rg" "git-grep")
                                   :language "elisp"
                                   :regex "\\\(defvarJJJ\\b\\s*"
                                   :tests ("(defvar test ")))
                 (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
                 (rule-failures (dumb-jump-test-ag-rules)))
            (should (= (length rule-failures) 1))))
      ;; restore `delete-trailing-whitespace'
      (advice-remove 'delete-trailing-whitespace 'dumb-jump--noop))))

(when (dumb-jump-rg-installed?)
  (ert-deftest dumb-jump-test-rg-rules-fail-test ()
    (let* ((bad-rule '(:type "variable"
                             :supports ("ag" "grep" "rg" "git-grep")
                             :language "elisp"
                             :regex "\\\(defvarJJJ\\b\\s*"
                             :tests ("(defvar test ")))
           ;; Add a bad rules to the rules normally used by dumb-jump.
	   (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
           ;; then test all rules to detect the ones that fail.
	   (rule-failures (dumb-jump-test-rg-rules)))
      ;; No rule taken from dumb-jump.el should fail: the only one that
      ;; should fail should be the `bad-rule' defined above.
      (should (= (length rule-failures) 1)))))

(when (and (not (eq system-type 'darwin))   ; git grep is broken on macOS as of Jan 2026.
           (dumb-jump-git-grep-installed?))
  (ert-deftest dumb-jump-test-git-grep-rules-fail-test ()
    (let* ((bad-rule '(:type "variable"
                             :supports ("ag" "grep" "rg" "git-grep")
                             :language "elisp"
                             :regex "\\\(defvarJJJ\\b\\s*"
                             :tests ("(defvar test ")))
	   (dumb-jump-find-rules (cons bad-rule dumb-jump-find-rules))
	   (rule-failures (dumb-jump-test-git-grep-rules)))
      (should (= (length rule-failures) 1)))))

(when (and (not (eq system-type 'darwin))  ; git grep is broken on macOS as of Jan 2026.
           (dumb-jump-git-grep-installed?))
  (ert-deftest dumb-jump-test-git-grep-rules-not-test () ;; :not tests
    (let ((rule-failures (dumb-jump-test-git-grep-rules t)))
    (dumb-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0)))))

(ert-deftest dumb-jump-match-test ()
  (should (not (dumb-jump-re-match nil "asdf")))
  (should (dumb-jump-re-match "^asdf$" "asdf"))
  (should (string= (car (dumb-jump-re-match "^[0-9]+$" "123")) "123")))

(ert-deftest dumb-jump-context-point-test ()
  (let* ((sentence "mainWindow.loadUrl('file://')")
         (func "loadUrl")
         (ctx (dumb-jump-get-point-context sentence func 11)))
         (should (string= (plist-get ctx :left) "mainWindow."))
         (should (string= (plist-get ctx :right) "('file://')"))))

(ert-deftest dumb-jump-context-point-type-test ()
  (let* ((sentence "mainWindow.loadUrl('file://' + __dirname + '/dt/inspector.html?electron=true');")
         (func "loadUrl")
         (pt-ctx (dumb-jump-get-point-context sentence func 11))
         (ctx-type (dumb-jump-get-ctx-type-by-language "javascript" pt-ctx)))
    (should (string= ctx-type "function"))))

(ert-deftest dumb-jump-prompt-user-for-choice-correct-test ()
  (let* ((results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()")
                    (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (popup-menu* *) => "/test2.txt:52: var thing = function()")
     (mock (dumb-jump-result-follow '(:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a")))
     (dumb-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumb-jump-prompt-user-for-choice-correct-helm-test ()
  (let* ((dumb-jump-selector 'helm)
         (results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()")
                    (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (helm-make-source "Jump to: " 'helm-source-sync :action * :candidates * :persistent-action *))
     (mock (helm * * :buffer "*helm dumb jump choices*"))
     (dumb-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumb-jump-prompt-user-for-choice-correct-helm-persistent-action-test ()
  (dumb-jump-helm-persist-action (list
                                  :path test-dumb-jump-file-path
                                  :line 1
                                  :context " (defn status"))
  (should (get-buffer " *helm dumb jump persistent*")))

(ert-deftest dumb-jump-prompt-user-for-choice-correct-ivy-test ()
  (let* ((dumb-jump-selector 'ivy)
         (dumb-jump-ivy-jump-to-selected-function
          #'dumb-jump-ivy-jump-to-selected)
         (results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()")
                    (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (ivy-read * * :action * :caller *)  => "/test2.txt:52: var thing = function()")
     (dumb-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumb-jump-a-back-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
        (mock (pop-tag-mark))
        (stub dumb-jump-rg-installed? => t)
        (with-no-warnings (dumb-jump-go))
        (with-no-warnings (dumb-jump-back))))))

(ert-deftest dumb-jump-fetch-results-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 10)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (let ((results (dumb-jump-fetch-file-results)))
          (should (string= "doSomeStuff" (plist-get results :symbol)))
          (should (string= "javascript" (plist-get results :lang))))))))

(ert-deftest dumb-jump-go-shell-test ()
  (with-current-buffer (get-buffer-create "*shell*")
    (insert ".js doSomeStuff()")
    (goto-char (point-min))
    (forward-char 6)
    (with-mock
      (stub dumb-jump-rg-installed? => t)
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
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 9))
        (should (string= go-js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-other-window-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 9))
        (should (string= go-js-file (with-no-warnings (dumb-jump-go-other-window))))))))

(ert-deftest dumb-jump-go-current-window-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 9))
        (should (string= go-js-file (with-no-warnings (dumb-jump-go-current-window))))))))

(ert-deftest dumb-jump-quick-look-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (popup-tip "/src/js/fake.js:3: function doSomeStuff() {"))
        (should (string= go-js-file (with-no-warnings (dumb-jump-quick-look))))))))

(ert-deftest dumb-jump-go-js2-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 11)
      (forward-char 76)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 7 35))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-js-es6a-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 1 4))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-js-es6b-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 21)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-js-es6c-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 5 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-js-es6d-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 23)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 10 2))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-js-es6e-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 24)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 16 2))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-js-es6-class-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 36)
      (forward-char 12)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 28 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))


(ert-deftest dumb-jump-go-sig-def-test ()
  (let ((dumb-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 7)
      (forward-char 35)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 6 25))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-sig-def2-test ()
  (let ((dumb-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 35)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 12 32))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-sig-def3-test ()
  (let ((dumb-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (forward-char 35)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 19 32))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-var-let-test ()
  (let ((dumb-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 33)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 11 10))
        (should (string= el-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-var-let-repeat-test ()
  (let ((dumb-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 21)
      (forward-char 33)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 18 10))
        (should (string= el-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-var-arg-test ()
  (let ((dumb-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 12)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 27))
        (should (string= el-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-no-result-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 4)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-message "'%s' %s %s declaration not found." "nothing" * *))
        (with-no-warnings (dumb-jump-go))))))

(ert-deftest dumb-jump-go-no-rules-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt")))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-message "Could not find rules for '%s'." ".txt file"))
        (with-no-warnings (dumb-jump-go))))))

(ert-deftest dumb-jump-go-too-long-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt"))
        (dumb-jump-max-find-time 0.2))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (noflet ((dumb-jump-fetch-file-results (&optional _prompt)
                 (with-no-warnings
                   (if (version< emacs-version "29.1")
                       (sleep-for 0 300)
                     (sleep-for 0.3)))
                 '(:results (:result))))
        (with-mock
          (stub dumb-jump-rg-installed? => t)
          (mock (dumb-jump-message "Took over %ss to find '%s'. Please install ag or rg, or add a .dumbjump file to '%s' with path exclusions" * * *))
          (mock (dumb-jump-result-follow * * *))
          (with-no-warnings (dumb-jump-go)))))))

(ert-deftest dumb-jump-message-handle-results-test ()
  (let ((dumb-jump-aggressive t)
        (results '((:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-goto-file-line "src/file.js" 62 4))
     (dumb-jump-handle-results results "src/file.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumb-jump-message-handle-results-choices-test ()
  (let ((results '((:path "src/file2.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file2.js" :line 63 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file2.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-prompt-user-for-choice "/code/redux" *))
     (dumb-jump-handle-results results "src/file.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumb-jump-grep-installed?-bsd-test ()
  (let ((dumb-jump--grep-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "grep (BSD grep) 2.5.1-FreeBSD\n" :times 1)
     (should (eq (dumb-jump-grep-installed?) 'bsd)))
     ;; confirm memoization of the previous result
     (should (eq (dumb-jump-grep-installed?) 'bsd))))

(ert-deftest dumb-jump-grep-installed?-gnu-test ()
  (let ((dumb-jump--grep-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "grep (GNU grep) 2.4.2\n" :times 1)
     (should (eq (dumb-jump-grep-installed?) 'gnu))
     ;; confirm memoization of the previous result
     (should (eq (dumb-jump-grep-installed?) 'gnu)))))

(ert-deftest dumb-jump-ag-installed?-test ()
  (let ((dumb-jump--ag-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "ag version 0.33.0\n" :times 1)
     (should (eq (dumb-jump-ag-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumb-jump-ag-installed?) t)))))

(ert-deftest dumb-jump-git-grep-plus-ag-installed?-test ()
  (let ((dumb-jump--git-grep-plus-ag-installed? 'unset)
        (dumb-jump--ag-installed? 'unset)
        (dumb-jump--git-grep-installed? 'unset))
    (with-mock
     ; this isn't ideal but combining the ag and git grep responses but this shouldn't matter in practice with :times 2
     (mock (shell-command-to-string *) => "ag version 0.33.0\nfatal: no pattern given\n" :times 2)
     (should (eq (dumb-jump-git-grep-plus-ag-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumb-jump-git-grep-plus-ag-installed?) t)))))

(ert-deftest dumb-jump-rg-installed?-test-no ()
  (let ((dumb-jump--rg-installed? 'unset))
    (with-mock
      (mock (executable-find *) => t)
      (mock (shell-command-to-string *) => "ripgrep 0.3.1\n" :times 1)
      (should (not (eq (dumb-jump-rg-installed?) t)))
      ;; confirm memoization of the previous result
      (should (not (eq (dumb-jump-rg-installed?) t))))))

(ert-deftest dumb-jump-rg-installed?-test-yes ()
  (let ((dumb-jump--rg-installed? 'unset))
    (with-mock
      (mock (executable-find *) => t)
      (mock (shell-command-to-string *) => "ripgrep 0.10.0\n\nfeatures:+pcre2\n\n" :times 1)
      (should (eq (dumb-jump-rg-installed?) t))
      ;; confirm memoization of the previous result
      (should (eq (dumb-jump-rg-installed?) t)))))

(ert-deftest dumb-jump-rg-installed?-test-old ()
  (let ((dumb-jump--rg-installed? 'unset))
    (with-mock
      (mock (executable-find *) => t)
      (mock (shell-command-to-string *) => "ripgrep 0.09.0\n\nfeatures:+pcre2\n\n" :times 1)
      (should (eq (dumb-jump-rg-installed?) nil))
      ;; confirm memoization of the previous result
      (should (eq (dumb-jump-rg-installed?) nil)))))

(ert-deftest dumb-jump-rg-installed?-test-yes-no-pcre2 ()
  (let ((dumb-jump--rg-installed? 'unset))
    (with-mock
      (mock (executable-find *) => t)
     (mock (shell-command-to-string *) => "ripgrep 0.10.0\n" :times 1)
     (should (eq (dumb-jump-rg-installed?) nil))
     ;; confirm memoization of the previous result
     (should (eq (dumb-jump-rg-installed?) nil)))))

(ert-deftest dumb-jump-rg-installed?-test-yes2 ()
  (let ((dumb-jump--rg-installed? 'unset))
    (with-mock
      (mock (executable-find *) => t)
      (mock (shell-command-to-string *) => "ripgrep 1.1.0\n\n\nfeatures:+pcre2\n" :times 1)
      (should (eq (dumb-jump-rg-installed?) t))
      ;; confirm memoization of the previous result
      (should (eq (dumb-jump-rg-installed?) t)))))

(ert-deftest dumb-jump-rg-installed?-test-yes2-no-pcre2 ()
  (let ((dumb-jump--rg-installed? 'unset))
    (with-mock
      (mock (executable-find *) => t)
      (mock (shell-command-to-string *) => "ripgrep 1.1.0\n" :times 1)
      (should (eq (dumb-jump-rg-installed?) nil))
      ;; confirm memoization of the previous result
      (should (eq (dumb-jump-rg-installed?) nil)))))

(ert-deftest dumb-jump-git-grep-installed?-test ()
  (let ((dumb-jump--git-grep-installed? 'unset))
    (with-mock
      (mock (shell-command-to-string *) => "fatal: no pattern given\n" :times 1)
      (should (eq (dumb-jump-git-grep-installed?) t))
      ;; confirm memoization of the previous result
      (should (eq (dumb-jump-git-grep-installed?) t)))))

(ert-deftest dumb-jump-go-nogrep-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
        ;; (mock (executable-find *) => t)
        (mock (dumb-jump-rg-installed?) => nil)
        (mock (dumb-jump-ag-installed?) => nil)
        (mock (dumb-jump-git-grep-installed?) => nil)
        (mock (dumb-jump-grep-installed?) => nil)
        (mock (dumb-jump-message "Please install ag, rg, git grep or grep!"))
        (with-no-warnings (dumb-jump-go))))))

(ert-deftest dumb-jump-go-nosymbol-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-message "No symbol under point."))
        (with-no-warnings (dumb-jump-go))))))

(ert-deftest dumb-jump-message-get-results-nogrep-test ()
  (with-mock
    ;; (mock (executable-find *) => t)
    (mock (dumb-jump-rg-installed?) => nil)
    (mock (dumb-jump-ag-installed?) => nil)
    (mock (dumb-jump-git-grep-installed?) => nil)
    (mock (dumb-jump-grep-installed?) => nil)
    (let ((results (dumb-jump-get-results)))
      (should (eq (plist-get results :issue) 'nogrep)))))

(ert-deftest dumb-jump-message-result-follow-test ()
  (with-mock
   (mock (dumb-jump-goto-file-line "src/file.js" 62 4))
   (let ((result '(:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
     (dumb-jump--result-follow result))))

(ert-deftest dumb-jump-message-result-follow-remote-fullpath-test ()
  (with-mock
    (mock (dumb-jump-goto-file-line * * *))
    (mock (file-remote-p *) => "/ssh:user@1.2.3.4#5678:")
    (let ((result '(:path "/usr/src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
      (should (string=
               (dumb-jump--result-follow result)
               "/ssh:user@1.2.3.4#5678:/usr/src/file.js")))))

(ert-deftest dumb-jump-message-result-follow-remote-relative-test ()
  (with-mock
    (mock (dumb-jump-goto-file-line * * *))
    (mock (file-remote-p *) => "/ssh:user@1.2.3.4#5678:")
    (let ((result '(:path "here/is/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow"))
          (default-directory "/ssh:user@1.2.3.4#5678:/path/to/default-directory/"))
      (should (string=
               (dumb-jump--result-follow result)
               "/ssh:user@1.2.3.4#5678:/path/to/default-directory/here/is/file.js")))))

(ert-deftest dumb-jump-message-result-follow-tooltip-test ()
  (with-mock
   (mock (popup-tip "/file.js:62: var isNow = true"))
   (let ((result '(:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
     (dumb-jump--result-follow result t "src"))))

(ert-deftest dumb-jump-populate-regexes-grep-test ()
  (should (equal (dumb-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'grep) '("testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'grep) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumb-jump-populate-regexes-ag-test ()
  (should (equal (dumb-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'ag) '("testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'ag) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumb-jump-populate-regexes-git-grep-plus-ag-test ()
  ;; this is effectively the same as `ag even with 'git-grep-plus-ag since that's where the regexes are used in this mode
  (should (equal (dumb-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'git-grep-plus-ag) '("testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'git-grep-plus-ag) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumb-jump-populate-regexes-rg-test ()
  (should (equal (dumb-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'rg) '("testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'rg) '("\\$testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "-testvar" '("JJJ\\s*=\\s*") 'rg) '("-testvar\\s*=\\s*"))))

(ert-deftest dumb-jump-populate-regexes-git-grep-test ()
  (should (equal (dumb-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'git-grep) '("testvar\\s*=\\s*")))
  (should (equal (dumb-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'git-grep) '("\\$testvar\\s*=\\s*"))))

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
        (result3 (dumb-jump-process-symbol-by-lang "ruby" ":myrubyfunc"))
        (result3b (dumb-jump-process-symbol-by-lang "ruby" "Health::Checks::QueueGrowth"))
        (result3c (dumb-jump-process-symbol-by-lang "ruby" "::Health"))
        (result4 (dumb-jump-process-symbol-by-lang "systemverilog" "`myvlfunc")))
    (should (string= result "somefunc"))
    (should (string= result2 "myfunc"))
    (should (string= result3 "myrubyfunc"))
    (should (string= result3b "QueueGrowth"))
    (should (string= result3c "Health"))
    (should (string= result4 "myvlfunc"))))

(ert-deftest dumb-jump--result-follow-test ()
  (let* ((data '(:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a")))
    (with-mock
     (mock (dumb-jump-goto-file-line "/usr/blah/test2.txt" 52 1))
     (dumb-jump--result-follow data nil "/usr/blah"))))

(ert-deftest dumb-jump-go-include-lib-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el"))
        (lib-file (f-join test-data-dir-elisp "../fake-library/lib.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 23)
      (forward-char 3)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 4 7))
        (should (string= (with-no-warnings (dumb-jump-go)) lib-file))))))

(ert-deftest dumb-jump-parse-response-line-test ()
  (let ((t1 (dumb-jump-parse-response-line "/opt/test/foo.js:44: var test = 12;" "/opt/test/blah.js"))
        (t1b (dumb-jump-parse-response-line "/path/to/file.f90:1701: subroutine test(foo)" "/path/to/file2.f90"))
        (t2 (dumb-jump-parse-response-line "47: var test = 13;" "/opt/test/blah.js"))
        (t3 (dumb-jump-parse-response-line "c:\\Users\\test\\foo.js:1: var test = 14;" "c:\\Users\\test\\foo.js"))
        (t4 (dumb-jump-parse-response-line "c:\\Users\\test\\foo2.js:2:test = {a:1,b:1};" "c:\\Users\\test\\foo.js"))
        (t5 (dumb-jump-parse-response-line "/opt/test/foo1.js:41: var test = {c:3, d: 4};" "/opt/test/b2.js")))
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

(ert-deftest dumb-jump-agtype-test ()
  (should (equal (dumb-jump-get-ag-type-by-language "python") '("python"))))

(ert-deftest dumb-jump-rgtype-test ()
  (should (equal (dumb-jump-get-rg-type-by-language "python") '("py"))))

(ert-deftest dumb-jump-git-grep-type-test ()
  (should (equal (dumb-jump-get-git-grep-type-by-language "python") '("py"))))

;; react tests

(ert-deftest dumb-jump-react-test1 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-react-test2 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 13 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))


(ert-deftest dumb-jump-react-test3 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 27)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 26 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-react-test4 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 32)
      (forward-char 7)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 31 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-react-test5 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 39)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 37 6))
        (should (string= js-file (with-no-warnings (dumb-jump-go))))))))

;; c++ tests

(ert-deftest dumb-jump-cpp-test1 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "test.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 14)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 3 6))
        (should (string= cpp-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-cpp-test2 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "test.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 9)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 1 6))
        (should (string= cpp-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-cpp-issue87 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "issue-87.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 16)
      (forward-char 12)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 6 18))
        (should (string= cpp-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-org-test1 ()
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "test.org"))
        (oldpar dumb-jump-force-searcher))
    (setq dumb-jump-force-searcher 'rg)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 9 6))
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)))

(ert-deftest dumb-jump-org-test2 ()
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "test.org"))
        (oldpar dumb-jump-force-searcher))
    (setq dumb-jump-force-searcher 'rg)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 14)
      (forward-char 10)
       (with-mock
         (stub dumb-jump-rg-installed? => t)
         (mock (dumb-jump-goto-file-line * 21 2))
         (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)))

(ert-deftest dumb-jump-org-issue135 ()
      (require 'org)
      (require 'ob-python)
      (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
      (let ((org-file (f-join test-data-dir-proj1 "src" "org" "test.org"))
            (oldpar dumb-jump-force-searcher)
            (oldproject dumb-jump-project))
        ;; in docker test AG version is 0.3 and 2.1.0 is needed
        (setq dumb-jump-force-searcher 'rg)
    ;; old version of org does not lead to point to the right directory
    (if (version< org-version "9")
        (setq dumb-jump-project (f-join test-data-dir-proj1 "src" "org")))
    (with-current-buffer (find-file-noselect org-file t)
     (goto-char (point-min))
      (forward-line 3)
      (forward-char 2)
      (org-edit-src-code)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 9 6))
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (if (version< org-version "9")
        (setq dumb-jump-project oldproject))))

(ert-deftest dumb-jump-org-named-block-test ()
  "Test jumping to org named block definition"
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "named-blocks.org"))
        (oldpar dumb-jump-force-searcher)
        (old-rg-installed dumb-jump--rg-installed?))
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump--rg-installed? 'unset)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 38)  ; Line 39: "Test for uniqueblock lookup: uniqueblock"
      (forward-char 34)  ; Position cursor on "uniqueblock" at end of line
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 41 8))  ; Line 41 is #+name: uniqueblock (col 8)
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (setq dumb-jump--rg-installed? old-rg-installed)))

(ert-deftest dumb-jump-org-heading-test ()
  "Test jumping to org heading"
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "named-blocks.org"))
        (oldpar dumb-jump-force-searcher)
        (old-rg-installed dumb-jump--rg-installed?))
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump--rg-installed? 'unset)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 65)  ; Line 66: "Reference to MainTopic heading: MainTopic"
      (forward-char 35)  ; Position cursor on "MainTopic" at end of line
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 68 4))  ; Line 68 is *** MainTopic (col 4)
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (setq dumb-jump--rg-installed? old-rg-installed)))

(ert-deftest dumb-jump-org-custom-id-test ()
  "Test jumping to org CUSTOM_ID"
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "named-blocks.org"))
        (oldpar dumb-jump-force-searcher)
        (old-rg-installed dumb-jump--rg-installed?))
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump--rg-installed? 'unset)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 57)  ; Line 58: "See also the section with ID call-section for more info."
      (forward-char 34)  ; Position cursor on "call-section"
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 55 12))  ; Line 55 is :CUSTOM_ID: call-section (col 12)
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (setq dumb-jump--rg-installed? old-rg-installed)))

(ert-deftest dumb-jump-org-call-test ()
  "Test jumping from #+call reference to named block definition"
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "named-blocks.org"))
        (oldpar dumb-jump-force-searcher)
        (old-rg-installed dumb-jump--rg-installed?))
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump--rg-installed? 'unset)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 61)  ; Line 62: "#+CALL: UPPERCASE()"
      (forward-char 8)   ; Position cursor on "UPPERCASE"
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 20 8))  ; Line 20 is #+NAME: UPPERCASE (col 8)
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (setq dumb-jump--rg-installed? old-rg-installed)))

(ert-deftest dumb-jump-org-named-block-nospace-test ()
  "Test jumping to org named block definition without space after colon"
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "named-blocks.org"))
        (oldpar dumb-jump-force-searcher)
        (old-rg-installed dumb-jump--rg-installed?))
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump--rg-installed? 'unset)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 45)  ; Line 46: "Test for nospace lookup: nospace"
      (forward-char 28)  ; Position cursor on "nospace" at end of line
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 48 7))  ; Line 48 is #+name:nospace (col 7)
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (setq dumb-jump--rg-installed? old-rg-installed)))

(ert-deftest dumb-jump-org-custom-id-nospace-test ()
  "Test jumping to org CUSTOM_ID without space after colon"
  (let ((org-file (f-join test-data-dir-proj1 "src" "org" "named-blocks.org"))
        (oldpar dumb-jump-force-searcher)
        (old-rg-installed dumb-jump--rg-installed?))
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump--rg-installed? 'unset)
    (with-current-buffer (find-file-noselect org-file t)
      (goto-char (point-min))
      (forward-line 87)  ; Line 88: "Reference to nospace-id custom ID: nospace-id"
      (forward-char 36)  ; Position cursor on "nospace-id" at end of line
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 85 11))  ; Line 85 is :CUSTOM_ID:nospace-id (col 11)
        (should (string= org-file (with-no-warnings (dumb-jump-go))))))
    (setq dumb-jump-force-searcher oldpar)
    (setq dumb-jump--rg-installed? old-rg-installed)))


;; This test verifies that having ".dumbjumpignore" files in the two sub-projects will make it find
;; the "multiproj" folder as project root since it has a ".dumbjump" file. The two sub-projects have
;; a dummy ".git" folder to signify it as a repository that would normally become the root without
;; the ignore file.
(ert-deftest dumb-jump-multiproj ()
  (let ((main-file (f-join test-data-dir-multiproj "subproj1" "main.cc"))
        (header-file (f-join test-data-dir-multiproj "subproj2" "header.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 18)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 6 6))
        (should (string= header-file (with-no-warnings (dumb-jump-go))))))))

;; This test makes sure that even though there's a local match it will jump to the external file
;; match instead.
(ert-deftest dumb-jump-prefer-external ()
  (let ((dumb-jump-aggressive t)
        (main-file (f-join test-data-dir-proj1 "src" "cpp" "external.cpp"))
        (header-file (f-join test-data-dir-proj1 "src" "cpp" "external.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 10)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 4 6))
        (should (string= header-file (with-no-warnings (dumb-jump-go-prefer-external))))))))

(ert-deftest dumb-jump-prefer-only-external ()
  (let ((main-file (f-join test-data-dir-multiproj "subproj1" "main.cc"))
        (header-file (f-join test-data-dir-multiproj "subproj2" "header.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 18)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 6 6))
        (should (string= header-file (with-no-warnings (dumb-jump-go-prefer-external))))))))

(ert-deftest dumb-jump-prefer-external-only-current ()
  (let ((main-file (f-join test-data-dir-proj1 "src" "cpp" "only.cpp")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 6 6))
        (should (string= main-file (with-no-warnings (dumb-jump-go-prefer-external))))))))

(ert-deftest dumb-jump-prefer-external-other-window ()
  (let ((dumb-jump-aggressive t)
        (main-file (f-join test-data-dir-proj1 "src" "cpp" "external.cpp"))
        (header-file (f-join test-data-dir-proj1 "src" "cpp" "external.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 10)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 4 6))
        (should (string= header-file (with-no-warnings (dumb-jump-go-prefer-external-other-window))))))))

(ert-deftest dumb-jump-filter-no-start-comments ()
  (should (equal '((:context "yield me"))
                 (dumb-jump-filter-no-start-comments '((:context "// filter me out")
                                                       (:context "yield me")) "c++"))))

(ert-deftest dumb-jump-filter-no-start-comments-unknown-language ()
  (should (equal nil (dumb-jump-filter-no-start-comments '() "unknownlanguage"))))

(defun generators-valid (pl searcher)
  (and (eq 8 (length pl))
       (functionp (plist-get pl :parse))
       (functionp (plist-get pl :generate))
       (functionp (plist-get pl :installed))
       (eq searcher (plist-get pl :searcher))))

(ert-deftest dumb-jump-generators-by-searcher-git-grep ()
  (let* ((searcher 'git-grep)
         (gen-funcs (dumb-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumb-jump-generators-by-searcher-ag ()
  (let* ((searcher 'ag)
         (gen-funcs (dumb-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumb-jump-generators-by-searcher-git-grep-plus-ag ()
  (let* ((searcher 'git-grep-plus-ag)
         (gen-funcs (dumb-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumb-jump-generators-by-searcher-rg ()
  (let* ((searcher 'rg)
         (gen-funcs (dumb-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumb-jump-generators-by-searcher-gnu-grep ()
  (let* ((searcher 'gnu-grep)
         (gen-funcs (dumb-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumb-jump-generators-by-searcher-grep ()
  (let* ((searcher 'grep)
         (gen-funcs (dumb-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(defun generator-plist-equal (pl1 pl2)
  (and (eq (length pl1) (length pl2))
       (eq (plist-get pl1 :parse)
           (plist-get pl2 :parse))
       (eq (plist-get pl1 :generate)
           (plist-get pl2 :generate))
       (eq (plist-get pl1 :installed)
           (plist-get pl2 :installed))
       (eq (plist-get pl1 :searcher)
           (plist-get pl2 :searcher))))

;; Test the search tool selection based on the 2 user-options:
;; - `dumb-jump-prefer-searcher'
;; - `dumb-jump-force-searcher' acting as an override
;; First run  simple tests that only check one combination.
(ert-deftest dumb-jump-selected-grep-variant-test-nil-nil-with-nothing ()
  (let ((dumb-jump-prefer-searcher nil)
        (dumb-jump-force-searcher nil))
    (with-mock
      ;; (mock (executable-find *) => t)
      (mock (dumb-jump-ag-installed?) => nil)
      (mock (dumb-jump-rg-installed?) => nil)
      (mock (dumb-jump-grep-installed?) => 'bsd)
      (should (eq (dumb-jump-selected-grep-variant) 'grep)))))

(ert-deftest dumb-jump-selected-grep-variant-test-nil-nil-with-really-nothing ()
  (let ((dumb-jump-prefer-searcher nil)
        (dumb-jump-force-searcher nil))
    (with-mock
      ;; (mock (executable-find *) => t)
      (mock (dumb-jump-ag-installed?) => nil)
      (mock (dumb-jump-rg-installed?) => nil)
      (mock (dumb-jump-grep-installed?) => nil)
      ;; [:todo 2026-01-14, by Pierre Rouleau: When dumb-jump-grep-installed?
      ;;                    returns nil the selection is still 'grep. Is this OK??]
      (should (eq (dumb-jump-selected-grep-variant) 'grep)))))

(ert-deftest dumb-jump-selected-grep-variant-test-nil-nil-with-ag ()
  (let ((dumb-jump-prefer-searcher nil)
        (dumb-jump-force-searcher nil))
    (with-mock
      (mock (dumb-jump-ag-installed?) => t)
      (should (eq (dumb-jump-selected-grep-variant) 'ag)))))

(ert-deftest dumb-jump-selected-grep-variant-test-nil-nil-with-rg ()
  (let ((dumb-jump-prefer-searcher nil)
        (dumb-jump-force-searcher nil))
    (with-mock
      ;; (mock (executable-find *) => t)
      (mock (dumb-jump-ag-installed?) => nil)
      (mock (dumb-jump-rg-installed?) => t)
      (should (eq (dumb-jump-selected-grep-variant) 'rg)))))

(ert-deftest dumb-jump-selected-grep-variant-test-ag-nil ()
  (let ((dumb-jump-prefer-searcher 'ag)
        (dumb-jump-force-searcher nil))
    (with-mock
      (mock (dumb-jump-ag-installed?) => t)
      (should (eq (dumb-jump-selected-grep-variant) 'ag)))))

(ert-deftest dumb-jump-selected-grep-variant-test-rg-nil ()
  (let ((dumb-jump-prefer-searcher 'rg)
        (dumb-jump-force-searcher nil))
    (with-mock
      (mock (dumb-jump-rg-installed?) => t)
      (should (eq (dumb-jump-selected-grep-variant) 'rg)))))

(ert-deftest dumb-jump-selected-grep-variant-test-rg-grep ()
  (let ((dumb-jump-prefer-searcher 'rg)
        (dumb-jump-force-searcher 'grep))
    (should (eq (dumb-jump-selected-grep-variant) 'grep))))

(ert-deftest dumb-jump-selected-grep-variant-test-grep-nil ()
  (let ((dumb-jump-prefer-searcher 'grep)
        (dumb-jump-force-searcher nil))
    (should (eq (dumb-jump-selected-grep-variant) 'grep))))

(ert-deftest dumb-jump-selected-grep-variant-test-gnu-grep-nil ()
  (let ((dumb-jump-prefer-searcher 'gnu-grep)
        (dumb-jump-force-searcher nil))
    (should (eq (dumb-jump-selected-grep-variant) 'gnu-grep))))

;; Run a test that go through all combinations

(defvar dumb-jump-search-selector-tester-choice nil
  "What `dumb-jump-search-selector-tester' returns.")
(defun dumb-jump-search-selector-tester (_dir)
  "Mocker for a user-supplied selector as override. Ignore DIR."
  dumb-jump-search-selector-tester-choice)

(ert-deftest dumb-jump-selected-grep-variant-tests ()
  (let ((dumb-jump-prefer-searcher nil)
        (dumb-jump-force-searcher nil))
    (with-mock
      ;; (mock (executable-find *) => t)
      (mock (dumb-jump-ag-installed?) => t)
      (mock (dumb-jump-rg-installed?) => t)
      (mock (dumb-jump-grep-installed?) => 'gnu)
      (mock (dumb-jump-git-grep-installed?) => t)
      ;; [:todo 2026-01-14, by Pierre Rouleau: the next one is never called.  Why?]
      ;; (mock (dumb-jump--git-grep-plus-ag-installed?) => t)

      (dolist (dumb-jump-prefer-searcher '(ag rg grep gnu-grep
                                              git-grep git-grep-plus-ag))
        ;;
        ;; When there is no overriding, the preference is honoured.
        (setq dumb-jump-force-searcher nil)
        (should (eq (dumb-jump-selected-grep-variant)
                    dumb-jump-prefer-searcher))
        ;;
        ;; when there is an overriding of the old/deprecated style,
        ;; the preference is ignored.
        (dolist (dumb-jump-force-searcher '(ag rg grep gnu-grep
                                               git-grep git-grep-plus-ag))
          (should (eq (dumb-jump-selected-grep-variant)
                      dumb-jump-force-searcher)))
        ;;
        ;; When the overriding is provided by a user-specified function, that
        ;; function determines the selection. Drive user selection via the
        ;; `dumb-jump-search-selector-tester-choice' variable in this test:
        (setq dumb-jump-force-searcher #'dumb-jump-search-selector-tester)
        (dolist (dumb-jump-search-selector-tester-choice '(ag rg grep gnu-grep
                                                              git-grep
                                                              git-grep-plus-ag))
          (should (eq (dumb-jump-selected-grep-variant)
                      dumb-jump-search-selector-tester-choice)))
        ;;
        ;; When the provided overriding is based on directories: the identified
        ;; directories in the list are requesting the use of git-grep but only
        ;; inside those directories
        (setq dumb-jump-force-searcher (list (f-expand ".")))
        (should (eq (dumb-jump-selected-grep-variant (f-expand "."))
                    'git-grep))
        ;;
        ;; When overriding for a directory that is not the current directory,
        ;; then this overriding does not take effect and the
        ;; `dumb-jump-prefer-searcher'  value is used.
        (setq dumb-jump-force-searcher '("/some/non/existing/directory"))
        (should (eq (dumb-jump-selected-grep-variant (f-expand "."))
                    dumb-jump-prefer-searcher))))))

;; --
(ert-deftest dumb-jump-pick-grep-variant-force ()
  (let* ((dumb-jump-force-searcher 'grep)
         (gen-funcs (dumb-jump-generators-by-searcher 'grep))
         (variant (dumb-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

;; The following test no longer represents what the searcher selection logic
;; does but what it used to do: always select git-grep when the project
;; directory is a Git repo even when `dumb-jump-force-searcher' was nil.
;; With the new logic, `dumb-jump-force-searcher' must be set to a user
;; function that selects Git Grep or the project directory is identified to
;; override it to Git Grep.
;; Therefore, the following test should be removed.
;; - For now I keep it to show the evolution.
;; The real test is now done by `dumb-jump-selected-grep-variant-tests' above.
;;
;; (ert-deftest dumb-jump-pick-grep-variant-git-grep-in-git-repo ()
;;   (let* ((dumb-jump-force-searcher nil)
;;          (gen-funcs (dumb-jump-generators-by-searcher 'git-grep))
;;          (variant (dumb-jump-pick-grep-variant (f-expand "."))))
;;     (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumb-jump-pick-grep-variant-prefer ()
  (let* ((dumb-jump-force-searcher nil)
         (dumb-jump-prefer-searcher 'grep)
         (gen-funcs (dumb-jump-generators-by-searcher 'grep))
         (variant (dumb-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumb-jump-pick-grep-variant-fallback-ag ()
  (let* ((dumb-jump-force-searcher nil)
         (dumb-jump-prefer-searcher nil)
	 (dumb-jump--ag-installed? t)
         (dumb-jump--rg-installed? nil)
         (gen-funcs (dumb-jump-generators-by-searcher 'ag))
         (variant (dumb-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumb-jump-pick-grep-variant-fallback-rg ()
  (let* ((dumb-jump-force-searcher nil)
         (dumb-jump-prefer-searcher nil)
         (dumb-jump--ag-installed? nil)
         (dumb-jump--rg-installed? t)
         (gen-funcs (dumb-jump-generators-by-searcher 'rg))
         (variant (dumb-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumb-jump-pick-grep-variant-fallback-gnu-grep ()
  (let* ((dumb-jump-force-searcher nil)
         (dumb-jump-prefer-searcher nil)
         (dumb-jump--ag-installed? nil)
         (dumb-jump--rg-installed? nil)
         (dumb-jump--grep-installed? 'gnu)
         (gen-funcs (dumb-jump-generators-by-searcher 'gnu-grep))
         (variant (dumb-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumb-jump-pick-grep-variant-fallback-grep ()
  (let* ((dumb-jump-force-searcher nil)
         (dumb-jump-prefer-searcher nil)
         (dumb-jump--ag-installed? nil)
         (dumb-jump--rg-installed? nil)
         (dumb-jump--grep-installed? 'bsd)
         (gen-funcs (dumb-jump-generators-by-searcher 'grep))
         (variant (dumb-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

;; This test makes sure that if the `cur-file' is absolute but results are relative, then it must
;; still find and sort results correctly.
(ert-deftest dumb-jump-handle-results-relative-current-file-test ()
  (let ((dumb-jump-aggressive t)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-goto-file-line "relfile.js" 62 4))
     (dumb-jump-handle-results results "/code/redux/relfile.js" "/code/redux" "" "isNow" nil nil))))

;; Make sure it jumps aggressively, i.e. normally.
(ert-deftest dumb-jump-handle-results-aggressively-test ()
  (let ((dumb-jump-aggressive t)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-goto-file-line "relfile.js" 62 4))
     (dumb-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

;; Make sure non-aggressive mode shows choices when more than one possibility.
(ert-deftest dumb-jump-handle-results-non-aggressively-test ()
  (let ((dumb-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-prompt-user-for-choice "/code/redux" *))
     (dumb-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumb-jump-handle-results-non-aggressively-quick-look-test ()
  (let ((dumb-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (popup-menu* '("relfile.js:62: var isNow = true" "src/absfile.js:69: isNow = false")) :times 1)
     (dumb-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" t nil))))

;; Make sure it jumps when there's only one possibility in non-aggressive mode.
(ert-deftest dumb-jump-handle-results-non-aggressive-do-jump-test ()
  (let ((dumb-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow"))))
    (with-mock
     (mock (dumb-jump-goto-file-line "relfile.js" 62 4))
     (dumb-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumb-jump-shell-command-switch-zsh-test ()
  (let ((shell-file-name "/usr/bin/zsh"))
    (should (string-equal "-icf" (dumb-jump-shell-command-switch)))))

(ert-deftest dumb-jump-shell-command-switch-csh-test ()
  (let ((shell-file-name "/usr/bin/csh"))
    (should (string-equal "-icf" (dumb-jump-shell-command-switch)))))

(ert-deftest dumb-jump-shell-command-switch-tcsh-test ()
  (let ((shell-file-name "/usr/bin/zsh"))
    (should (string-equal "-icf" (dumb-jump-shell-command-switch)))))

(ert-deftest dumb-jump-shell-command-switch-bash-test ()
  (let ((shell-file-name "/usr/bin/bash"))
    (should (string-equal "-c" (dumb-jump-shell-command-switch)))))

(ert-deftest dumb-jump-shell-command-switch-unknown-test ()
  (let ((shell-file-name "/usr/bin/thisshelldoesnotexist"))
    (should (string-equal shell-command-switch (dumb-jump-shell-command-switch)))))

(ert-deftest dumb-jump-go-clojure-question-mark-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file2.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode)  ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 1 6))
        (should (string= clj-to-file (with-no-warnings (dumb-jump-go))))))))


(ert-deftest dumb-jump-go-clojure-no-question-mark-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file1.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 2 6))
        (should (string= clj-to-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-clojure-no-asterisk-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file2.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 2)
      (with-mock
        ;; (stub dumb-jump-ag-installed? => t)
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 4 9))
        (should (string= clj-to-file (with-no-warnings (dumb-jump-go))))))))

(ert-deftest dumb-jump-go-clojure-asterisk-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file1.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 5)
      (forward-char 2)
      (with-mock
        (stub dumb-jump-rg-installed? => t)
        (mock (dumb-jump-goto-file-line * 5 7))
        (should (string= clj-to-file (with-no-warnings (dumb-jump-go))))))))


(ert-deftest dumb-jump-format-files-as-ag-arg-test ()
  (let* ((fake-files '("one" "two" "three"))
         (result (dumb-jump-format-files-as-ag-arg fake-files "path"))
         (expected "'(path/one|path/two|path/three)'"))
    (should (string= result expected))))

(ert-deftest dumb-jump-get-git-grep-files-matching-symbol-test ()
  (with-mock
   (mock (shell-command-to-string "git grep --full-name -F -c symbol path") => "fileA:1\nfileB:2\n")
   (should (equal (dumb-jump-get-git-grep-files-matching-symbol "symbol" "path") '("fileA" "fileB")))))

(ert-deftest dumb-jump-get-git-grep-files-matching-symbol-as-ag-arg-test ()
  (with-mock
   (mock (shell-command-to-string "git grep --full-name -F -c symbol path") => "fileA:1\nfileB:2\n")
   (should (string= (dumb-jump-get-git-grep-files-matching-symbol-as-ag-arg "symbol" "path") "'(path/fileA|path/fileB)'"))))

;;;
