;;; -*- lexical-binding: t -*-
(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
(require 'popup)
;;; Code:

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

(ert-deftest dumber-jump-language-to-ext-test ()
  (should (-contains? (dumber-jump-get-file-exts-by-language "elisp") "el")))

(ert-deftest dumber-jump-generate-cmd-include-args ()
  (let ((args (dumber-jump-get-ext-includes "javascript"))
        (expected " --include \\*.js --include \\*.jsx --include \\*.vue --include \\*.html --include \\*.css "))
    (should (string= expected args))))

(ert-deftest dumber-jump-generate-grep-command-no-ctx-test ()
  (let* ((system-type 'darwin)
         (regexes (dumber-jump-get-contextual-regexes "elisp" nil 'grep))
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  '("\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(tester\\s+")))
         (expected (concat "LANG=C grep -REn --include \\*.el --include \\*.el.gz" (s-join "" expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-grep-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-gnu-grep-command-no-ctx-test ()
  (let* ((system-type 'darwin)
         (regexes (dumber-jump-get-contextual-regexes "elisp" nil 'gnu-grep))
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  '("\\((defun|cl-defun)[[:space:]]+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defmacro[[:space:]]+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defvar\\b[[:space:]]*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defcustom\\b[[:space:]]*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(setq\\b[[:space:]]*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(tester[[:space:]]+")))
         (expected (concat "LANG=C grep -rEn" (s-join "" expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-gnu-grep-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-ag-command-no-ctx-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes "\\((defun|cl-defun)\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester(?![a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (expected (concat "ag --nocolor --nogroup --elisp " (shell-quote-argument expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-ag-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-ag-command-exclude-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes "\\((defun|cl-defun)\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester(?![a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (expected (concat "ag --nocolor --nogroup --elisp --ignore-dir this/is/excluded " (shell-quote-argument expected-regexes) " /path/to/proj-root")))
    (should (string= expected  (dumber-jump-generate-ag-command  "tester" "blah.el" "/path/to/proj-root" regexes "elisp" '("/path/to/proj-root/this/is/excluded"))))))

(ert-deftest dumber-jump-generate-git-grep-plus-ag-command-no-ctx-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes "\\((defun|cl-defun)\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester(?![a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (expected (concat "ag --nocolor --nogroup -G '(/path/to/proj-root/blah.el)' " (shell-quote-argument expected-regexes) " /path/to/proj-root"))) ;; NOTE no "--elisp" and the `-G` arg is new
  (with-mock
   (mock (dumber-jump-get-git-grep-files-matching-symbol-as-ag-arg * *) => "'(/path/to/proj-root/blah.el)'")
    (should (string= expected  (dumber-jump-generate-git-grep-plus-ag-command  "tester" "blah.el" "/path/to/proj-root" regexes "elisp" nil))))))


(ert-deftest dumber-jump-generate-git-grep-plus-ag-command-exclude-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes "\\((defun|cl-defun)\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester(?![a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (expected (concat "ag --nocolor --nogroup -G '(/path/to/proj-root/blah.el)' --ignore-dir this/is/excluded " (shell-quote-argument expected-regexes) " /path/to/proj-root"))) ;; NOTE no "--elisp" and the `-G` arg is new
  (with-mock
   (mock (dumber-jump-get-git-grep-files-matching-symbol-as-ag-arg * *) => "'(/path/to/proj-root/blah.el)'")
    (should (string= expected  (dumber-jump-generate-git-grep-plus-ag-command  "tester" "blah.el" "/path/to/proj-root" regexes "elisp" '("/path/to/proj-root/this/is/excluded")))))))


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

(ert-deftest dumber-jump-generate-git-grep-command-no-ctx-test ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'git-grep))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (excludes '("one" "two" "three"))
         (expected (concat "git grep --color=never --line-number --untracked -E " (shell-quote-argument expected-regexes) " -- ./\\*.el ./\\*.el.gz \\:\\(exclude\\)one \\:\\(exclude\\)two \\:\\(exclude\\)three")))
    (should (string= expected  (dumber-jump-generate-git-grep-command  "tester" "blah.el" "." regexes "elisp" excludes)))))

(ert-deftest dumber-jump-generate-git-grep-command-no-ctx-extra-args ()
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'git-grep))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (excludes '("one" "two" "three"))
         (dumber-jump-git-grep-search-args "--recurse-submodules")
         (expected (concat "git grep --color=never --line-number --untracked --recurse-submodules -E " (shell-quote-argument expected-regexes) " -- ./\\*.el ./\\*.el.gz \\:\\(exclude\\)one \\:\\(exclude\\)two \\:\\(exclude\\)three")))
    (should (string= expected  (dumber-jump-generate-git-grep-command  "tester" "blah.el" "." regexes "elisp" excludes)))))

(ert-deftest dumber-jump-generate-ag-command-no-ctx-extra-args ()
  ;; ag args
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'ag))
         (expected-regexes "\\((defun|cl-defun)\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester(?![a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester(?![a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester(?![a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (dumber-jump-ag-search-args "--follow")
         (expected (concat "ag --nocolor --nogroup --follow --elisp " (shell-quote-argument expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-ag-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-rg-command-no-ctx-extra-args ()
  ;; rg-args
  (let* ((regexes (dumber-jump-get-contextual-regexes "elisp" nil 'rg))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (dumber-jump-rg-search-args "--no-pcre2 --follow")
         (expected (concat "rg --color never --no-heading --line-number -U --no-pcre2 --follow --type elisp " (shell-quote-argument expected-regexes) " .")))
    (should (string= expected  (dumber-jump-generate-rg-command  "tester" "blah.el" "." regexes "elisp" nil)))))

(ert-deftest dumber-jump-generate-git-grep-command-not-search-untracked-test ()
  (let* ((dumber-jump-git-grep-search-args "")
         (dumber-jump-git-grep-search-untracked nil)
         (regexes (dumber-jump-get-contextual-regexes "elisp" nil 'git-grep))
         (expected-regexes "\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])|\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])|\\(tester\\s+|\\((defun|cl-defun)\\s*.+\\(?\\s*tester($|[^a-zA-Z0-9\\?\\*-])\\s*\\)?")
         (excludes '("one" "two" "three"))
         (expected (concat "git grep --color=never --line-number -E " (shell-quote-argument expected-regexes) " -- ./\\*.el ./\\*.el.gz \\:\\(exclude\\)one \\:\\(exclude\\)two \\:\\(exclude\\)three")))
    (should (string= expected  (dumber-jump-generate-git-grep-command  "tester" "blah.el" "." regexes "elisp" excludes)))))

(ert-deftest dumber-jump-generate-grep-command-no-ctx-funcs-only-test ()
  (let* ((system-type 'darwin)
         (dumber-jump-functions-only t)
         (regexes (dumber-jump-get-contextual-regexes "elisp" nil 'grep))
         (expected-regexes (s-join ""
                            (--map (concat " -e " (shell-quote-argument it))
                                   '("\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                     "\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])"))))
         (expected (concat "LANG=C grep -REn" expected-regexes " ."))
         (zexpected (concat "LANG=C zgrep -REn" expected-regexes " .")))
    (should (string= expected  (dumber-jump-generate-grep-command  "tester" "blah.el" "." regexes "" nil)))
    (should (string= zexpected  (dumber-jump-generate-grep-command  "tester" "blah.el.gz" "." regexes "" nil)))))

(ert-deftest dumber-jump-generate-grep-command-with-ctx-test ()
  (let* ((system-type 'darwin)
         (ctx-type (dumber-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
         (dumber-jump-ignore-context nil) ;; overriding the default
         (regexes (dumber-jump-get-contextual-regexes "elisp" ctx-type 'grep))
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  '("\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])")))
         (expected (concat "LANG=C grep -REn" (s-join "" expected-regexes) " .")))
    ;; the point context being passed should match a "function" type so only the one command
    (should (string= expected  (dumber-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil)))))

(ert-deftest dumber-jump-generate-grep-command-on-windows-test ()
  (noflet ((shell-quote-argument (it) (format "'%s'" it)))
    (let* ((system-type 'windows-nt)
           (ctx-type (dumber-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
           (dumber-jump-ignore-context nil) ;; overriding the default
           (regexes (dumber-jump-get-contextual-regexes "elisp" ctx-type 'grep))
           (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                    '("\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                      "\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])")))
           (expected (concat "grep -REn" (s-join "" expected-regexes) " .")))
      (should (string= expected  (dumber-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil))))))

(ert-deftest dumber-jump-generate-grep-command-with-ctx-but-ignored-test ()
  (let* ((system-type 'darwin)
         (ctx-type (dumber-jump-get-ctx-type-by-language "elisp" '(:left "(" :right nil)))
         (dumber-jump-ignore-context t)
         (regexes (dumber-jump-get-contextual-regexes "elisp" ctx-type nil))
         (expected-regexes (--map (concat " -e " (shell-quote-argument it))
                                  '("\\((defun|cl-defun)\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defmacro\\s+tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defvar\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(defcustom\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(setq\\b\\s*tester($|[^a-zA-Z0-9\\?\\*-])"
                                    "\\(tester\\s+")))
         (expected (concat "LANG=C grep -REn" (s-join "" expected-regexes) " .")))

    ;; the point context being passed is ignored so ALL should return
    (should (string= expected  (dumber-jump-generate-grep-command "tester" "blah.el" "." regexes "" nil)))))

(ert-deftest dumber-jump-generate-bad-grep-command-test ()
    (should (s-blank? (dumber-jump-generate-grep-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumber-jump-generate-bad-ag-command-test ()
    (should (s-blank? (dumber-jump-generate-ag-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumber-jump-generate-bad-rg-command-test ()
  (should (s-blank? (dumber-jump-generate-rg-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumber-jump-generate-bad-git-grep-command-test ()
    (should (s-blank? (dumber-jump-generate-git-grep-command "tester" "blah.el" "." nil "" (list "skaldjf")))))

(ert-deftest dumber-jump-grep-parse-test ()
  (let* ((resp "./dumber-jump.el:22:(defun dumber-jump-asdf ()\n./dumber-jump.el:26:(defvar some-var )\n./dumber-jump2.el:28:(defvar some-var)")
         (parsed (dumber-jump-parse-grep-response resp "dumber-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumber-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumber-jump-grep-parse-no-filter-test ()
  (let* ((resp "./dumber-jump.el:22:(defun dumber-jump-asdf ()\n./dumber-jump.el:26:(defvar some-var )\n")
         (parsed (dumber-jump-parse-grep-response resp "dumber-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumber-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumber-jump-ag-parse-test ()
  (let* ((resp "./dumber-jump.el:22:(defun dumber-jump-asdf ()\n./dumber-jump.el:26:(defvar some-var )\n./dumber-jump2.el:28:1:(defvar some-var)")
         (parsed (dumber-jump-parse-ag-response resp "dumber-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumber-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumber-jump-rg-parse-test ()
  (let* ((resp "./dumber-jump.el:22:(defun dumber-jump-asdf ()\n./dumber-jump.el:26:(defvar some-var )\n./dumber-jump2.el:28:1:(defvar some-var)")
         (parsed (dumber-jump-parse-rg-response resp "dumber-jump2.el" 28))
         (test-result (nth 1 parsed)))
    (should (= (plist-get test-result :diff) 2))
    (should (= (length parsed) 2))
    (should (string= (plist-get test-result :path) "dumber-jump.el"))
    (should (= (plist-get test-result ':line) 26))))

(ert-deftest dumber-jump-git-grep-parse-test ()
  (let* ((resp "./dumber-jump.el:22:(defun dumber-jump-asdf ()\n./dumber-jump.el:26:(defvar some-var )\n./dumber-jump2.el:28:1:(defvar some-var)")
         (parsed (dumber-jump-parse-git-grep-response resp "dumber-jump2.el" 28))
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

;; (ert-deftest dumber-jump-run-grep-cmd-test ()
;;   (let* ((dumber-jump-force-grep t)
;;          (regexes (dumber-jump-get-contextual-regexes "elisp" nil))
;;          (results (dumber-jump-run-command "another-fake-function" test-data-dir-elisp regexes "" ""  "blah.el" 3))
;;         (first-result (car results)))
;;     (should (s-contains? "/fake.el" (plist-get first-result :path)))
;;     (should (= (plist-get first-result :line) 6))))

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

(ert-deftest dumber-jump-goto-file-line-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-mock
     (mock (ring-insert * *))
     (dumber-jump-goto-file-line js-file 3 0)
     (should (string= (buffer-file-name) js-file))
     (should (= (line-number-at-pos) 3)))))

(ert-deftest dumber-jump-test-grep-rules-test ()
  (let ((rule-failures (dumber-jump-test-grep-rules)))
    (dumber-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0))))

(when (dumber-jump-ag-installed?)
  (ert-deftest dumber-jump-test-ag-rules-test ()
    (let ((rule-failures (dumber-jump-test-ag-rules)))
      (dumber-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(when (dumber-jump-rg-installed?)
  (ert-deftest dumber-jump-test-rg-rules-test ()
    (let ((rule-failures (dumber-jump-test-rg-rules)))
      (dumber-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(when (dumber-jump-git-grep-installed?)
  (ert-deftest dumber-jump-test-git-grep-rules-test ()
    (let ((rule-failures (dumber-jump-test-git-grep-rules)))
      (dumber-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(ert-deftest dumber-jump-test-grep-rules-not-test () ;; :not tests
  (let ((rule-failures (dumber-jump-test-grep-rules t)))
    (dumber-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0))))

(when (dumber-jump-ag-installed?)
  (ert-deftest dumber-jump-test-ag-rules-not-test () ;; :not tests
    (let ((rule-failures (dumber-jump-test-ag-rules t)))
    (dumber-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0)))))

(when (dumber-jump-rg-installed?)
  (ert-deftest dumber-jump-test-rg-rules-not-test () ;; :not tests
    (let ((rule-failures (dumber-jump-test-rg-rules t)))
      (dumber-jump-output-rule-test-failures rule-failures)
      (should (= (length rule-failures) 0)))))

(ert-deftest dumber-jump-test-grep-rules-fail-test ()
  (let* ((bad-rule '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
         (dumber-jump-find-rules (cons bad-rule dumber-jump-find-rules))
         (rule-failures (dumber-jump-test-grep-rules)))
    (should (= (length rule-failures) 1))))

(when (dumber-jump-ag-installed?)
  (ert-deftest dumber-jump-test-ag-rules-fail-test ()
    (let* ((bad-rule '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
           (dumber-jump-find-rules (cons bad-rule dumber-jump-find-rules))
           (rule-failures (dumber-jump-test-ag-rules)))
      (should (= (length rule-failures) 1)))))

(when (dumber-jump-rg-installed?)
  (ert-deftest dumber-jump-test-rg-rules-fail-test ()
    (let* ((bad-rule '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
	   (dumber-jump-find-rules (cons bad-rule dumber-jump-find-rules))
	   (rule-failures (dumber-jump-test-rg-rules)))
      (should (= (length rule-failures) 1)))))

(when (dumber-jump-git-grep-installed?)
  (ert-deftest dumber-jump-test-git-grep-rules-fail-test ()
    (let* ((bad-rule '(:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp" :regex "\\\(defvarJJJ\\b\\s*" :tests ("(defvar test ")))
	   (dumber-jump-find-rules (cons bad-rule dumber-jump-find-rules))
	   (rule-failures (dumber-jump-test-git-grep-rules)))
      (should (= (length rule-failures) 1)))))

(when (dumber-jump-git-grep-installed?)
  (ert-deftest dumber-jump-test-git-grep-rules-not-test () ;; :not tests
    (let ((rule-failures (dumber-jump-test-git-grep-rules t)))
    (dumber-jump-output-rule-test-failures rule-failures)
    (should (= (length rule-failures) 0)))))

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

(ert-deftest dumber-jump-prompt-user-for-choice-correct-test ()
  (let* ((results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()")
                    (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (popup-menu* *) => "/test2.txt:52: var thing = function()")
     (mock (dumber-jump-result-follow '(:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a")))
     (dumber-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumber-jump-prompt-user-for-choice-correct-helm-test ()
  (let* ((dumber-jump-selector 'helm)
         (results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()")
                    (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (helm-make-source "Jump to: " 'helm-source-sync :action * :candidates * :persistent-action *))
     (mock (helm * * :buffer "*helm dumber jump choices*"))
     (dumber-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumber-jump-prompt-user-for-choice-correct-helm-persistent-action-test ()
  (dumber-jump-helm-persist-action '(:path "dumber-jump.el" :line 1 :context " (defn status"))
  (should (get-buffer " *helm dumber jump persistent*")))

(ert-deftest dumber-jump-prompt-user-for-choice-correct-ivy-test ()
  (let* ((dumber-jump-selector 'ivy)
         (dumber-jump-ivy-jump-to-selected-function
          #'dumber-jump-ivy-jump-to-selected)
         (results '((:path "/usr/blah/test.txt" :line 54 :context "function thing()")
                    (:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a"))))
    (with-mock
     (mock (ivy-read * * :action * :caller *)  => "/test2.txt:52: var thing = function()")
     (dumber-jump-prompt-user-for-choice "/usr/blah" results))))

(ert-deftest dumber-jump-a-back-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (pop-tag-mark))
       (dumber-jump-go)
       (dumber-jump-back)))))

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
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 9))
       (should (string= go-js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-other-window-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 9))
       (should (string= go-js-file (dumber-jump-go-other-window)))))))

(ert-deftest dumber-jump-go-current-window-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 9))
       (should (string= go-js-file (dumber-jump-go-current-window)))))))

(ert-deftest dumber-jump-quick-look-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js"))
        (go-js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (popup-tip "/src/js/fake.js:3: function doSomeStuff() {"))
       (should (string= go-js-file (dumber-jump-quick-look)))))))

(ert-deftest dumber-jump-go-js2-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 11)
      (forward-char 76)
      (with-mock
       (mock (dumber-jump-goto-file-line * 7 35))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-js-es6a-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (with-mock
       (mock (dumber-jump-goto-file-line * 1 4))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-js-es6b-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 21)
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 6))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-js-es6c-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (with-mock
       (mock (dumber-jump-goto-file-line * 5 6))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-js-es6d-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 23)
      (with-mock
       (mock (dumber-jump-goto-file-line * 10 2))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-js-es6e-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 24)
      (with-mock
       (mock (dumber-jump-goto-file-line * 16 2))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-js-es6-class-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "es6.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 36)
      (forward-char 12)
      (with-mock
       (mock (dumber-jump-goto-file-line * 28 6))
       (should (string= js-file (dumber-jump-go)))))))


(ert-deftest dumber-jump-go-sig-def-test ()
  (let ((dumber-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 7)
      (forward-char 35)
      (with-mock
       (mock (dumber-jump-goto-file-line * 6 25))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-sig-def2-test ()
  (let ((dumber-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 35)
      (with-mock
       (mock (dumber-jump-goto-file-line * 12 32))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-sig-def3-test ()
  (let ((dumber-jump-aggressive t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 20)
      (forward-char 35)
      (with-mock
       (mock (dumber-jump-goto-file-line * 19 32))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-var-let-test ()
  (let ((dumber-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 13)
      (forward-char 33)
      (with-mock
       (mock (dumber-jump-goto-file-line * 11 10))
       (should (string= el-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-var-let-repeat-test ()
  (let ((dumber-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 21)
      (forward-char 33)
      (with-mock
       (mock (dumber-jump-goto-file-line * 18 10))
       (should (string= el-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-var-arg-test ()
  (let ((dumber-jump-aggressive t)
        (el-file (f-join test-data-dir-elisp "fake2.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 12)
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 27))
       (should (string= el-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-no-result-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 4)
      (with-mock
       (mock (dumber-jump-message "'%s' %s %s declaration not found." "nothing" * *))
       (dumber-jump-go)))))

(ert-deftest dumber-jump-go-no-result-force-grep-test ()
  (let ((dumber-jump-force-grep t)
        (js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 4)
      (with-mock
       (mock (dumber-jump-message "'%s' %s %s declaration not found." "nothing" * *))
       (dumber-jump-go)))))

(ert-deftest dumber-jump-go-no-rules-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt")))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (with-mock
       (mock (dumber-jump-message "Could not find rules for '%s'." ".txt file"))
       (dumber-jump-go)))))

(ert-deftest dumber-jump-go-too-long-test ()
  (let ((txt-file (f-join test-data-dir-proj1 "src" "js" "nocode.txt"))
        (dumber-jump-max-find-time 0.2))
    (with-current-buffer (find-file-noselect txt-file t)
      (goto-char (point-min))
      (noflet ((dumber-jump-fetch-file-results (&optional prompt)
                 (sleep-for 0 300)
                 '(:results (:result))))
        (with-mock
          (mock (dumber-jump-message "Took over %ss to find '%s'. Please install ag or rg, or add a .dumbjump file to '%s' with path exclusions" * * *))
          (mock (dumber-jump-result-follow * * *))
          (dumber-jump-go))))))

(ert-deftest dumber-jump-message-handle-results-test ()
  (let ((dumber-jump-aggressive t)
        (results '((:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-goto-file-line "src/file.js" 62 4))
     (dumber-jump-handle-results results "src/file.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumber-jump-message-handle-results-choices-test ()
  (let ((results '((:path "src/file2.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file2.js" :line 63 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/file2.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-prompt-user-for-choice "/code/redux" *))
     (dumber-jump-handle-results results "src/file.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumber-jump-grep-installed?-bsd-test ()
  (let ((dumber-jump--grep-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "grep (BSD grep) 2.5.1-FreeBSD\n" :times 1)
     (should (eq (dumber-jump-grep-installed?) 'bsd)))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-grep-installed?) 'bsd))))

(ert-deftest dumber-jump-grep-installed?-gnu-test ()
  (let ((dumber-jump--grep-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "grep (GNU grep) 2.4.2\n" :times 1)
     (should (eq (dumber-jump-grep-installed?) 'gnu))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-grep-installed?) 'gnu)))))

(ert-deftest dumber-jump-ag-installed?-test ()
  (let ((dumber-jump--ag-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "ag version 0.33.0\n" :times 1)
     (should (eq (dumber-jump-ag-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-ag-installed?) t)))))

(ert-deftest dumber-jump-git-grep-plus-ag-installed?-test ()
  (let ((dumber-jump--git-grep-plus-ag-installed? 'unset)
        (dumber-jump--ag-installed? 'unset)
        (dumber-jump--git-grep-installed? 'unset))
    (with-mock
     ; this isn't ideal but combining the ag and git grep responses but this shouldn't matter in practice with :times 2
     (mock (shell-command-to-string *) => "ag version 0.33.0\nfatal: no pattern given\n" :times 2)
     (should (eq (dumber-jump-git-grep-plus-ag-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-git-grep-plus-ag-installed?) t)))))

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

(ert-deftest dumber-jump-git-grep-installed?-test ()
  (let ((dumber-jump--git-grep-installed? 'unset))
    (with-mock
     (mock (shell-command-to-string *) => "fatal: no pattern given\n" :times 1)
     (should (eq (dumber-jump-git-grep-installed?) t))
     ;; confirm memoization of the previous result
     (should (eq (dumber-jump-git-grep-installed?) t)))))

(ert-deftest dumber-jump-go-nogrep-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-char 13)
      (with-mock
       (mock (dumber-jump-rg-installed?) => nil)
       (mock (dumber-jump-ag-installed?) => nil)
       (mock (dumber-jump-git-grep-installed?) => nil)
       (mock (dumber-jump-grep-installed?) => nil)
       (mock (dumber-jump-message "Please install ag, rg, git grep or grep!"))
       (dumber-jump-go)))))

(ert-deftest dumber-jump-go-nosymbol-test ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "fake2.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 1)
      (with-mock
       (mock (dumber-jump-message "No symbol under point."))
       (dumber-jump-go)))))

(ert-deftest dumber-jump-message-get-results-nogrep-test ()
  (with-mock
   (mock (dumber-jump-rg-installed?) => nil)
   (mock (dumber-jump-ag-installed?) => nil)
   (mock (dumber-jump-git-grep-installed?) => nil)
   (mock (dumber-jump-grep-installed?) => nil)
   (let ((results (dumber-jump-get-results)))
     (should (eq (plist-get results :issue) 'nogrep)))))

(ert-deftest dumber-jump-message-result-follow-test ()
  (with-mock
   (mock (dumber-jump-goto-file-line "src/file.js" 62 4))
   (let ((result '(:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
     (dumber-jump--result-follow result))))

(ert-deftest dumber-jump-message-result-follow-remote-fullpath-test ()
  (with-mock
    (mock (dumber-jump-goto-file-line * * *))
    (mock (file-remote-p *) => "/ssh:user@1.2.3.4#5678:")
    (let ((result '(:path "/usr/src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
      (should (string=
               (dumber-jump--result-follow result)
               "/ssh:user@1.2.3.4#5678:/usr/src/file.js")))))

(ert-deftest dumber-jump-message-result-follow-remote-relative-test ()
  (with-mock
    (mock (dumber-jump-goto-file-line * * *))
    (mock (file-remote-p *) => "/ssh:user@1.2.3.4#5678:")
    (let ((result '(:path "here/is/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow"))
          (default-directory "/ssh:user@1.2.3.4#5678:/path/to/default-directory/"))
      (should (string=
               (dumber-jump--result-follow result)
               "/ssh:user@1.2.3.4#5678:/path/to/default-directory/here/is/file.js")))))

(ert-deftest dumber-jump-message-result-follow-tooltip-test ()
  (with-mock
   (mock (popup-tip "/file.js:62: var isNow = true"))
   (let ((result '(:path "src/file.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")))
     (dumber-jump--result-follow result t "src"))))

(ert-deftest dumber-jump-populate-regexes-grep-test ()
  (should (equal (dumber-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'grep) '("testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'grep) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumber-jump-populate-regexes-ag-test ()
  (should (equal (dumber-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'ag) '("testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'ag) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumber-jump-populate-regexes-git-grep-plus-ag-test ()
  ;; this is effectively the same as `ag even with 'git-grep-plus-ag since that's where the regexes are used in this mode
  (should (equal (dumber-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'git-grep-plus-ag) '("testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'git-grep-plus-ag) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumber-jump-populate-regexes-rg-test ()
  (should (equal (dumber-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'rg) '("testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'rg) '("\\$testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "-testvar" '("JJJ\\s*=\\s*") 'rg) '("[-]testvar\\s*=\\s*"))))

(ert-deftest dumber-jump-populate-regexes-git-grep-test ()
  (should (equal (dumber-jump-populate-regexes "testvar" '("JJJ\\s*=\\s*") 'git-grep) '("testvar\\s*=\\s*")))
  (should (equal (dumber-jump-populate-regexes "$testvar" '("JJJ\\s*=\\s*") 'git-grep) '("\\$testvar\\s*=\\s*"))))

(ert-deftest dumber-jump-message-prin1-test ()
  (with-mock
   (mock (message "%s %s" "(:path \"test\" :line 24)" "3"))
   (dumber-jump-message-prin1 "%s %s" '(:path "test" :line 24) 3)))

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

(ert-deftest dumber-jump--result-follow-test ()
  (let* ((data '(:path "/usr/blah/test2.txt" :line 52 :context "var thing = function()" :target "a")))
    (with-mock
     (mock (dumber-jump-goto-file-line "/usr/blah/test2.txt" 52 1))
     (dumber-jump--result-follow data nil "/usr/blah"))))

(ert-deftest dumber-jump-go-include-lib-test ()
  (let ((el-file (f-join test-data-dir-elisp "fake2.el"))
        (lib-file (f-join test-data-dir-elisp "../fake-library/lib.el")))
    (with-current-buffer (find-file-noselect el-file t)
      (goto-char (point-min))
      (forward-line 23)
      (forward-char 3)
      (with-mock
       (mock (dumber-jump-goto-file-line * 4 7))
        (should (string= (dumber-jump-go) lib-file))))))

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

(ert-deftest dumber-jump-agtype-test ()
  (should (equal (dumber-jump-get-ag-type-by-language "python") '("python"))))

(ert-deftest dumber-jump-rgtype-test ()
  (should (equal (dumber-jump-get-rg-type-by-language "python") '("py"))))

(ert-deftest dumber-jump-git-grep-type-test ()
  (should (equal (dumber-jump-get-git-grep-type-by-language "python") '("py"))))

;; react tests

(ert-deftest dumber-jump-react-test1 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 6))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-react-test2 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 22)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 13 6))
       (should (string= js-file (dumber-jump-go)))))))


(ert-deftest dumber-jump-react-test3 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 27)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 26 6))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-react-test4 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 32)
      (forward-char 7)
      (with-mock
       (mock (dumber-jump-goto-file-line * 31 6))
       (should (string= js-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-react-test5 ()
  (let ((js-file (f-join test-data-dir-proj1 "src" "js" "react.js")))
    (with-current-buffer (find-file-noselect js-file t)
      (goto-char (point-min))
      (forward-line 39)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 37 6))
       (should (string= js-file (dumber-jump-go)))))))

;; c++ tests

(ert-deftest dumber-jump-cpp-test1 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "test.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 14)
      (with-mock
       (mock (dumber-jump-goto-file-line * 3 6))
       (should (string= cpp-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-cpp-test2 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "test.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 8)
      (forward-char 9)
      (with-mock
       (mock (dumber-jump-goto-file-line * 1 6))
       (should (string= cpp-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-cpp-issue87 ()
  (let ((cpp-file (f-join test-data-dir-proj1 "src" "cpp" "issue-87.cpp")))
    (with-current-buffer (find-file-noselect cpp-file t)
      (goto-char (point-min))
      (forward-line 16)
      (forward-char 12)
      (with-mock
       (mock (dumber-jump-goto-file-line * 6 18))
       (should (string= cpp-file (dumber-jump-go)))))))

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
      (with-mock
       (mock (dumber-jump-goto-file-line * 6 6))
       (should (string= header-file (dumber-jump-go)))))))

;; This test makes sure that even though there's a local match it will jump to the external file
;; match instead.
(ert-deftest dumber-jump-prefer-external ()
  (let ((dumber-jump-aggressive t)
        (main-file (f-join test-data-dir-proj1 "src" "cpp" "external.cpp"))
        (header-file (f-join test-data-dir-proj1 "src" "cpp" "external.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 10)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 4 6))
       (should (string= header-file (dumber-jump-go-prefer-external)))))))

(ert-deftest dumber-jump-prefer-only-external ()
  (let ((main-file (f-join test-data-dir-multiproj "subproj1" "main.cc"))
        (header-file (f-join test-data-dir-multiproj "subproj2" "header.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 18)
      (with-mock
       (mock (dumber-jump-goto-file-line * 6 6))
       (should (string= header-file (dumber-jump-go-prefer-external)))))))

(ert-deftest dumber-jump-prefer-external-only-current ()
  (let ((main-file (f-join test-data-dir-proj1 "src" "cpp" "only.cpp")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 6 6))
       (should (string= main-file (dumber-jump-go-prefer-external)))))))

(ert-deftest dumber-jump-prefer-external-other-window ()
  (let ((dumber-jump-aggressive t)
        (main-file (f-join test-data-dir-proj1 "src" "cpp" "external.cpp"))
        (header-file (f-join test-data-dir-proj1 "src" "cpp" "external.h")))
    (with-current-buffer (find-file-noselect main-file t)
      (goto-char (point-min))
      (forward-line 10)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 4 6))
       (should (string= header-file (dumber-jump-go-prefer-external-other-window)))))))

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

(ert-deftest dumber-jump-generators-by-searcher-git-grep ()
  (let* ((searcher 'git-grep)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumber-jump-generators-by-searcher-ag ()
  (let* ((searcher 'ag)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumber-jump-generators-by-searcher-git-grep-plus-ag ()
  (let* ((searcher 'git-grep-plus-ag)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumber-jump-generators-by-searcher-rg ()
  (let* ((searcher 'rg)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumber-jump-generators-by-searcher-gnu-grep ()
  (let* ((searcher 'gnu-grep)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
    (should (generators-valid gen-funcs searcher))))

(ert-deftest dumber-jump-generators-by-searcher-grep ()
  (let* ((searcher 'grep)
         (gen-funcs (dumber-jump-generators-by-searcher searcher)))
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

(ert-deftest dumber-jump-pick-grep-variant-force ()
  (let* ((dumber-jump-force-searcher 'grep)
         (gen-funcs (dumber-jump-generators-by-searcher 'grep))
         (variant (dumber-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumber-jump-pick-grep-variant-git-grep-in-git-repo ()
  (let* ((dumber-jump-force-searcher nil)
         (gen-funcs (dumber-jump-generators-by-searcher 'git-grep))
         (variant (dumber-jump-pick-grep-variant (f-expand "."))))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumber-jump-pick-grep-variant-prefer ()
  (let* ((dumber-jump-force-searcher nil)
         (dumber-jump-prefer-searcher 'grep)
         (gen-funcs (dumber-jump-generators-by-searcher 'grep))
         (variant (dumber-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumber-jump-pick-grep-variant-fallback-ag ()
  (let* ((dumber-jump-force-searcher nil)
         (dumber-jump-prefer-searcher nil)
	  (dumber-jump--ag-installed? t)
         (dumber-jump--rg-installed? nil)
         (gen-funcs (dumber-jump-generators-by-searcher 'ag))
         (variant (dumber-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumber-jump-pick-grep-variant-fallback-rg ()
  (let* ((dumber-jump-force-searcher nil)
         (dumber-jump-prefer-searcher nil)
         (dumber-jump--ag-installed? nil)
         (dumber-jump--rg-installed? t)
         (gen-funcs (dumber-jump-generators-by-searcher 'rg))
         (variant (dumber-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumber-jump-pick-grep-variant-fallback-gnu-grep ()
  (let* ((dumber-jump-force-searcher nil)
         (dumber-jump-prefer-searcher nil)
         (dumber-jump--ag-installed? nil)
         (dumber-jump--rg-installed? nil)
         (dumber-jump--grep-installed? 'gnu)
         (gen-funcs (dumber-jump-generators-by-searcher 'gnu-grep))
         (variant (dumber-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

(ert-deftest dumber-jump-pick-grep-variant-fallback-grep ()
  (let* ((dumber-jump-force-searcher nil)
         (dumber-jump-prefer-searcher nil)
         (dumber-jump--ag-installed? nil)
         (dumber-jump--rg-installed? nil)
         (dumber-jump--grep-installed? 'bsd)
         (gen-funcs (dumber-jump-generators-by-searcher 'grep))
         (variant (dumber-jump-pick-grep-variant)))
    (should (generator-plist-equal gen-funcs variant))))

;; This test makes sure that if the `cur-file' is absolute but results are relative, then it must
;; still find and sort results correctly.
(ert-deftest dumber-jump-handle-results-relative-current-file-test ()
  (let ((dumber-jump-aggressive t)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-goto-file-line "relfile.js" 62 4))
     (dumber-jump-handle-results results "/code/redux/relfile.js" "/code/redux" "" "isNow" nil nil))))

;; Make sure it jumps aggressively, i.e. normally.
(ert-deftest dumber-jump-handle-results-aggressively-test ()
  (let ((dumber-jump-aggressive t)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-goto-file-line "relfile.js" 62 4))
     (dumber-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

;; Make sure non-aggressive mode shows choices when more than one possibility.
(ert-deftest dumber-jump-handle-results-non-aggressively-test ()
  (let ((dumber-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (dumber-jump-prompt-user-for-choice "/code/redux" *))
     (dumber-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" nil nil))))

(ert-deftest dumber-jump-handle-results-non-aggressively-quick-look-test ()
  (let ((dumber-jump-aggressive nil)
        (results '((:path "relfile.js" :line 62 :context "var isNow = true" :diff 7 :target "isNow")
                   (:path "src/absfile.js" :line 69 :context "isNow = false" :diff 0 :target "isNow"))))
    (with-mock
     (mock (popup-menu* '("relfile.js:62: var isNow = true" "src/absfile.js:69: isNow = false")) :times 1)
     (dumber-jump-handle-results results "relfile.js" "/code/redux" "" "isNow" t nil))))

;; Make sure it jumps when there's only one possibility in non-aggressive mode.
(ert-deftest dumber-jump-handle-results-non-aggressive-do-jump-test ()
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
      (with-mock
       (mock (dumber-jump-goto-file-line * 1 6))
       (should (string= clj-to-file (dumber-jump-go)))))))


(ert-deftest dumber-jump-go-clojure-no-question-mark-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file1.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 3)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 2 6))
       (should (string= clj-to-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-clojure-no-asterisk-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file2.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 4)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 4 9))
       (should (string= clj-to-file (dumber-jump-go)))))))

(ert-deftest dumber-jump-go-clojure-asterisk-test ()
  (let ((clj-jump-file (f-join test-data-dir-proj3 "file3.clj"))
        (clj-to-file (f-join test-data-dir-proj3 "file1.clj")))
    (with-current-buffer (find-file-noselect clj-jump-file t)
      (funcall 'lisp-mode) ;; need a built in lisp mode so `?` is part of a symbol
      (goto-char (point-min))
      (forward-line 5)
      (forward-char 2)
      (with-mock
       (mock (dumber-jump-goto-file-line * 5 7))
       (should (string= clj-to-file (dumber-jump-go)))))))


(ert-deftest dumber-jump-format-files-as-ag-arg-test ()
  (let* ((fake-files '("one" "two" "three"))
         (result (dumber-jump-format-files-as-ag-arg fake-files "path"))
         (expected "'(path/one|path/two|path/three)'"))
    (should (string= result expected))))

(ert-deftest dumber-jump-get-git-grep-files-matching-symbol-test ()
  (with-mock
   (mock (shell-command-to-string "git grep --full-name -F -c symbol path") => "fileA:1\nfileB:2\n")
   (should (equal (dumber-jump-get-git-grep-files-matching-symbol "symbol" "path") '("fileA" "fileB")))))

(ert-deftest dumber-jump-get-git-grep-files-matching-symbol-as-ag-arg-test ()
  (with-mock
   (mock (shell-command-to-string "git grep --full-name -F -c symbol path") => "fileA:1\nfileB:2\n")
   (should (string= (dumber-jump-get-git-grep-files-matching-symbol-as-ag-arg "symbol" "path") "'(path/fileA|path/fileB)'"))))

;;;
