;;; dumb-jump.el --- Dumb jumping to variable and function definitions

;; Copyright (C) 2015 jack angers
;; Author: jack angers
;; Version: 1.0
;; Package-Requires: ((f "0.17.3") (s "1.9.0") (dash "2.9.0"))
;; Keywords: programming
;;; Commentary:

;; Dumb Jump is an Emacs "jump to definition" package with support for multiple programming languages that favors
;; "just working" over speed or accuracy.  This means minimal -- and ideally zero -- configuration with absolutely
;; no stored indexes (TAGS) or persistent background processes.

;;; Code:
(require 'f)
(require 's)
(require 'dash)

(defgroup dumb-jump nil
  "Easily jump to project function and variable definitions"
  :group 'tools
  :group 'convenience)

;;;###autoload
(defvar dumb-jump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-g") 'dumb-jump-go )
    (define-key map (kbd "C-M-p") 'dumb-jump-back)
    map))

(defcustom dumb-jump-grep-prefix
  "LANG=C"
  "Prefix to grep command. Seemingly makes it faster for pure text."
  :group 'dumb-jump)

(defcustom dumb-jump-grep-cmd
  "grep"
  "The path to grep. By default assumes it is in path."
  :group 'dumb-jump)

(defcustom dumb-jump-ag-prefix
  ""
  "A prefix for when using ag"
  :group 'dumb-jump)

(defcustom dumb-jump-ag-cmd
  "ag"
  "The the path to the silver searcher. By default assumes it is in path. If not found  WILL fallback to grep"
  :group 'dumb-jump)

(defcustom dumb-jump-force-grep
  nil
  "When t will use grep even if ag is available"
  :group 'dumb-jump)

(defcustom dumb-jump-zgrep-cmd
  "zgrep"
  "Prefix to grep command. Seemingly makes it faster for pure text."
  :group 'dumb-jump)

(defcustom dumb-jump-grep-args
  "-REn"
  "Grep command args Recursive, [e]xtended regexes, and show line numbers"
  :group 'dumb-jump)

(defcustom dumb-jump-max-find-time
  2
  "Number of seconds a grep/find command can take before being warned"
  :group 'dumb-jump)

(defcustom dumb-jump-functions-only
  nil
  "Should we only jump to functions?"
  :group 'dumb-jump)

(defcustom dumb-jump-quiet
  nil
  "Should dumb-jump say what it is doing to message"
  :group 'dumb-jump)

(defcustom dumb-jump-ignore-context
  nil
  "Should we ignore context when jumping?"
  :group 'dumb-jump)

(defcustom dumb-jump-last-location
  '()
  "History of last locations when jumping"
  :group 'dumb-jump)

(defcustom dumb-jump-find-rules
  '((:type "function" :language "elisp" :regex "\\\(defun\\s+JJJ\\b\\s*"
           :tests ("(defun test (blah)" "(defun test\n"))

    (:type "variable" :language "elisp"
           :regex "\\\(defvar\\b\\s*JJJ\\b\\s?" :tests ("(defvar test " "(defvar test\n"))

    (:type "variable" :language "elisp"
           :regex "\\\(defcustom\\b\\s*JJJ\\b\\s?" :tests ("(defcustom test " "(defcustom test\n"))

    (:type "variable" :language "elisp"
           :regex "\\\(setq\\b\\s*JJJ\\b\\s*" :tests ("(setq test 123)"))
    (:type "variable" :language "elisp"
           :regex "\\\(JJJ\\s+" :tests ("(let ((test 123)))"))

    (:type "variable" :language "elisp"
           :regex "\\(defun\\s*.+\\\(?\\s*JJJ\\b\\s*\\\)?"
           :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)"))

    ;; python
    (:type "variable" :language "python"
           :regex "\\s*JJJ\\s*=\\s*" :tests ("test = 1234"))

    (:type "function" :language "python"
           :regex "def\\s*JJJ\\s*\\\("
           :tests ("\tdef test(asdf)" "def test()"))

    (:type "type" :language "python"
           :regex "class\\s*JJJ\\s*\\\(?"
           :tests ("class test(object):"))

    ;; ruby
    (:type "variable" :language "ruby"
           :regex "\\s*JJJ\\s*=\\s*" :tests ("test = 1234"))

    (:type "function" :language "ruby"
           :regex "\\bdef\\s*JJJ\\s*\\\("
           :tests ("def test(asdf)" "def test()"))

    ;; php
    (:type "function" :language "php"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))
    (:type "variable" :language "php"
           :regex "JJJ\\s*=\\s*"
           :tests ("$test = 1234"))

    ;; faust
    (:type "function" :language "faust"
           :regex "^\\s*JJJ\\((.+)\\)*\\s*=" :tests ("test = 1234"))

    ;; go
    (:type "variable" :language "go"
           :regex "\\s*\\bJJJ\\s*=\\s*" :tests ("test = 1234"))

    (:type "variable" :language "go"
           :regex "\\s*\\bJJJ\\s*:=\\s*" :tests ("test := 1234"))

    (:type "function" :language "go"
           :regex "func\\s+\\\([^\\\)]*\\\)\\s+JJJ\\s*\\\("
           :tests ("func (s *blah) test(filename string) string {"))

    (:type "function" :language "go"
           :regex "func\\s+JJJ\\s*\\\("
           :tests ("func test(url string) (string, error)"))

    (:type "type" :language "go"
           :regex "type\\s+JJJ\\s+struct\\s+\\\{"
           :tests ("type test struct {"))

    ;; javascript
    (:type "variable" :language "javascript"
           :regex "\\s*\\bJJJ\\s*=\\s*" :tests ("test = 1234"))

    (:type "variable" :language "javascript"
           :regex "\\bfunction\\s*[^\\(]*\\\(\\s*.*JJJ\\s*,?\\s*\\\)?"
           :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)"))
    (:type "function" :language "javascript"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))
    (:type "function" :language "javascript"
           :regex "\\s*\\bJJJ\\s*:\\s*function\\s*\\\("
           :tests ("test: function()"))
    (:type "function" :language "javascript"
           :regex "\\s*\\bJJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()")) )

  "List of regex patttern templates organized by language
and type to use for generating the grep command"
  :group 'dumb-jump)

(defcustom dumb-jump-language-file-exts
  '((:language "elisp" :ext "el")
    (:language "elisp" :ext "el.gz")
    (:language "faust" :ext "dsp")
    (:language "faust" :ext "lib")
    (:language "javascript" :ext "js")
    (:language "javascript" :ext "jsx")
    (:language "javascript" :ext "html")
    (:language "php" :ext "php")
    (:language "php" :ext "inc")
    (:language "ruby" :ext "rb")
    (:language "python" :ext "py")
    (:language "go" :ext "go"))
  "Mapping of programming lanaguage(s) to file extensions"
  :group 'dumb-jump)

(defcustom dumb-jump-language-contexts
  '((:language "javascript" :type "function" :right "^(" :left nil)
    (:language "javascript" :type "variable" :right nil :left "($")
    (:language "javascript" :type "variable" :right "^)" :left "($")
    (:language "javascript" :type "variable" :right "^\\." :left nil)
    (:language "javascript" :type "variable" :right "^;" :left nil)

    (:language "elisp" :type "function" :right nil :left "($")
    (:language "elisp" :type "variable" :right "^)" :left nil))

  "List of under points contexts for each language. This helps limit
the number of regular expressions we use if we know that if there's a '('
immediately to the right of a symbol then it's probably a function call"
  :group 'dumb-jump)

(defcustom dumb-jump-project-denoters
  '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "PkgInfo" "-pkg.el")
  "Files and directories that signify a directory is a project root"
  :group 'dumb-jump)

(defcustom dumb-jump-default-project "~"
  "The default project to search for searching if a denoter is not found in parent of file"
  :group 'dumb-jump)

(defun dumb-jump-message-prin1 (str &rest args)
  "Helper function when debuging applies prin1-to-string to all ARGS"
  (apply 'message str (-map 'prin1-to-string args)))

(defun dumb-jump-ag-installed? ()
  (s-contains? "ag version" (shell-command-to-string (concat dumb-jump-ag-cmd " --version"))))

(defun dumb-jump-find-start-pos (line-in look-for cur-pos)
  "Find start column position of LOOK-FOR in LINE-IN using CUR-POS as a hint"
  (let ((is-found nil)
        (line (s-replace "\t" (s-repeat tab-width " ") line-in)))
    (while (and (> cur-pos 0) (not is-found))
      (let* ((char (substring line cur-pos (1+ cur-pos)))
             (is-at (s-index-of char look-for)))
        (if (null is-at)
            (setq is-found t)
          (setq cur-pos (1- cur-pos)))))
    (1+ cur-pos)))

(defun dumb-jump-test-rules ()
  "Test all the rules and return count of those that fail
Optionally pass t to see a list of all failed rules"
  (let ((failures '())
        (fail-tmpl "FAILURE '%s' not in response '%s' | CMD: '%s' | rule: '%s'"))
    (-each dumb-jump-find-rules
      (lambda (rule)
        (-each (plist-get rule :tests)
          (lambda (test)
            (let* ((cmd (concat " echo '" test "' | grep -En -e '"  (s-replace "JJJ" "test" (plist-get rule :regex)) "'"))
                   (resp (shell-command-to-string cmd)))
              (when (not (s-contains? test resp))
                (add-to-list 'failures (format fail-tmpl test resp cmd rule))))
                ))))
    failures))

(defun dumb-jump-message (str &rest args)
  "Log message to the *Messages* buffer if not using dumb-jump-quiet"
  (when (not dumb-jump-quiet)
    (apply 'message str args)))

(defun dumb-jump-get-point-context (line func cur-pos)
  "Get the context to the left and right of FUNC in LINE using CUR-POS as hint"
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

(defun dumb-jump-generate-prompt-text (look-for proj results)
  (let* ((title (format "Multiple results for '%s':\n\n" look-for))
         (choices (-map-indexed (lambda (index result)
                                  (format "%d. %s:%s" (1+ index)
                                          (s-replace proj "" (plist-get result :path))
                                          (plist-get result :line)))
                                results)))
    (concat title (s-join "\n" choices) "\n\nChoice: ")))

(defun dumb-jump-parse-input (total input)
  (let* ((choice (string-to-number input)))
    (when (and
           (<= choice total)
           (>= choice 1))
      choice)))

(defun dumb-jump-prompt-user-for-choice (look-for proj results)
  (let* ((prompt-text (dumb-jump-generate-prompt-text look-for proj results))
         (input (read-from-minibuffer prompt-text))
         (choice (dumb-jump-parse-input (length results) input)))
    (if choice
      (dumb-jump-result-follow (nth (1- choice) results))
      (dumb-jump-message "Sorry, that's an invalid choice."))))

(defun dumb-jump-get-project-root (filepath)
  "Keep looking at the parent dir of FILEPATH until a
denoter file/dir is found or uses dumb-jump-default-profile"
  (f-expand
    (or
      (locate-dominating-file filepath #'dumb-jump-get-config)
      dumb-jump-default-project)))

(defun dumb-jump-get-config (dir)
  (car (--filter
          (f-exists? (f-join dir it))
        dumb-jump-project-denoters)))

(defun dumb-jump-get-language-by-filename (file)
  "Get the programming language from the FILENAME"
  (let* ((filename (if (s-ends-with? ".gz" file)
                       (f-no-ext file)
                     file))
         (result (-filter
                 (lambda (f) (s-ends-with? (concat "." (plist-get f :ext)) filename))
                 dumb-jump-language-file-exts)))
    (if result
        (plist-get (car result) :language)
      (format ".%s file" (f-ext file)))))

(defun dumb-jump-fetch-results ()
  "Build up a list of results by examining the current context and calling grep"
  (let* ((cur-file (buffer-file-name))
         (cur-line (thing-at-point 'line t))
         (cur-line-num (line-number-at-pos))
         (look-for (thing-at-point 'symbol t))
         (proj-root (dumb-jump-get-project-root cur-file))
         (proj-config (dumb-jump-get-config proj-root))
         (lang (dumb-jump-get-language-by-filename cur-file))
;         (pt-ctx (dumb-jump-get-point-context cur-line look-for))
         (pt-ctx (if (not (string= cur-line look-for))
                     (dumb-jump-get-point-context cur-line look-for (current-column))
                 nil))
         (ctx-type
          (dumb-jump-get-ctx-type-by-language lang pt-ctx))
         (regexes (dumb-jump-get-contextual-regexes lang ctx-type))
         ;(include-args (dumb-jump-get-ext-includes lang))
         (exclude-args (if (s-ends-with? ".dumbjump" proj-config)
                           (dumb-jump-read-exclusions proj-root proj-config)
                           nil))
         (raw-results (dumb-jump-run-command look-for proj-root regexes lang exclude-args cur-file cur-line-num))
         (results (-map (lambda (r) (plist-put r :target look-for)) raw-results)))
    `(:results ,results :lang ,(if (null lang) "" lang) :symbol ,look-for :ctx-type ,(if (null ctx-type) "" ctx-type) :file ,cur-file :root ,proj-root)))

(defun dumb-jump-back ()
  "Jump back to where the last jump was done"
  (interactive)
  (if dumb-jump-last-location
    (let* ((last-loc (car dumb-jump-last-location))
           (path (plist-get last-loc :path))
           (point (plist-get last-loc :point)))
      (dumb-jump-message "Jumping back to%s line %s"
               (if (not (string= path (buffer-file-name)))
                   (concat " " (f-filename path))
                 "")
               (number-to-string (plist-get last-loc :line)))
      (setq dumb-jump-last-location (cdr dumb-jump-last-location))
      (dumb-jump-goto-file-point path point))
    (dumb-jump-message "Nowhere to jump back to.")))

(defun dumb-jump-go () ; TODO: rename to dumb-jump-to-definition,
  "Go to the function/variable declaration for thing at point"
  (interactive)
  (let* ((start-time (float-time))
         (info (dumb-jump-fetch-results))
         (end-time (float-time))
         (fetch-time (- end-time start-time))
         (results (plist-get info :results))
         (look-for (plist-get info :symbol))
         (proj-root (plist-get info :root))
         (lang (plist-get info :lang))
         (result-count (length results)))
    ; (dumb-jump-message-prin1 "lang:%s type:%s results: %s" lang ctx-type results)
    (cond
;     ((and (not (listp results)) (s-blank? results))
     ((> fetch-time dumb-jump-max-find-time)
      (dumb-jump-message "Took over %ss to find '%s'. Please install ag or add a .dumbjump file to '%s' with path exclusions"
               (number-to-string dumb-jump-max-find-time) look-for proj-root))
     ((s-ends-with? " file" lang)
      (dumb-jump-message "Could not find rules for '%s'." lang))
     ((= result-count 1)
      (dumb-jump-result-follow (car results)))
     ((> result-count 1)
      ;; multiple results so let the user pick from a list
      ;; unless the match is in the current file
      (dumb-jump-handle-results results (plist-get info :file) proj-root (plist-get info :ctx-type) look-for))
     ((= result-count 0)
      (dumb-jump-message "'%s' %s %s declaration not found." look-for lang (plist-get info :ctx-type)))
     (t
      (dumb-jump-message "Un-handled results: %s " (prin1-to-string results))))))

(defun dumb-jump-handle-results (results cur-file proj-root ctx-type look-for)
  "Figure which of the RESULTS to jump to. Favoring the CUR-FILE"
  (let* ((match-sorted (-sort (lambda (x y) (< (plist-get x :diff) (plist-get y :diff))) results))
        ; moves current file results to the front of the list
        (match-cur-file-front (-concat
                               (--filter (and (> (plist-get it :diff) 0)
                                              (string= (plist-get it :path) cur-file)) match-sorted)

                               (--filter (and (<= (plist-get it :diff) 0)
                                              (string= (plist-get it :path) cur-file)) match-sorted)

                               (--filter (not (string= (plist-get it :path) cur-file)) match-sorted)))

        (matches (dumb-jump-current-file-results cur-file match-cur-file-front))
        (var-to-jump (car matches))
        ;; TODO: handle if ctx-type is null but ALL results are variable
        (do-var-jump (and (or (= (length matches) 1) (string= ctx-type "variable") (string= ctx-type "")) var-to-jump)))
    ;(dumb-jump-message-prin1 "type: %s | jump? %s | matches: %s | sorted: %s | results: %s" ctx-type var-to-jump matches match-sorted results)
    (if do-var-jump
        (dumb-jump-result-follow var-to-jump)
      (dumb-jump-prompt-user-for-choice look-for proj-root match-cur-file-front))))

(defun dumb-jump-read-exclusions (root config-file)
  "Build up the exclude-dir argument of the grep command by reading the config file"
  (let* ((contents (f-read-text (f-join root config-file)))
         (lines (s-split "\n" contents))
         (exclude-lines (-filter (lambda (f) (s-starts-with? "-" f)) lines))
         (exclude-paths (-map (lambda (f)
                                 (let* ((dir (substring f 1))
                                       (use-dir (if (s-starts-with? "/" dir)
                                                    (substring dir 1)
                                                    dir)))
                                   (f-join root use-dir)))
                               exclude-lines)))
    ;; (if exclude-lines
    ;;   (dumb-jump-arg-joiner "--exclude-dir" exclude-paths)
    ;;   "")))
    (if exclude-lines
        exclude-paths
      nil)))

(defun dumb-jump-result-follow (result)
  "Take the RESULT to jump to and record the jump, for jumping back, and then trigger jump."
  (let ((pos (s-index-of (plist-get result :target) (plist-get result :context)))
        (thef (plist-get result :path))
        (line (plist-get result :line)))
    (when thef
      (add-to-list 'dumb-jump-last-location
                   `(:line ,(line-number-at-pos) :path ,(buffer-file-name) :pos ,pos :point ,(point)))
      (dumb-jump-goto-file-line thef line pos))))

(defun dumb-jump-goto-file-line (thefile theline pos)
  "Open THEFILE and go line THELINE"
  ;(dumb-jump-message "Jumping to file '%s' line %s" thefile theline)
  (unless (string= thefile (buffer-file-name))
    (find-file thefile))
  (goto-char (point-min))
  (forward-line (1- theline))
  (forward-char pos))

(defun dumb-jump-goto-file-point (thefile point)
  "Open THEFILE and goto  POINT"
  (unless (string= thefile (buffer-file-name))
    (find-file thefile))
  (goto-char point))

(defun dumb-jump-current-file-results (path results)
  "Return the RESULTS that have the PATH"
  (let ((matched (-filter (lambda (r) (string= path (plist-get r :path))) results)))
    matched))

(defun dumb-jump-run-command (look-for proj regexes lang exclude-args cur-file line-num)
  "Run the grep command based on the needle LOOKFOR in the directory TOSEARCH"
  (let* ((has-ag (dumb-jump-ag-installed?))
         (use-grep (or (not has-ag) dumb-jump-force-grep))
         (cmd (if use-grep
                  (dumb-jump-generate-grep-command look-for cur-file proj regexes lang exclude-args)
                  (dumb-jump-generate-ag-command look-for cur-file proj regexes lang exclude-args)))
         (rawresults (shell-command-to-string cmd)))
    ;(dumb-jump-message-prin1 "RUNNING CMD '%s' RESULTS: %s" cmd rawresults)
    (unless (s-blank? cmd)
      (if use-grep
          (dumb-jump-parse-grep-response rawresults cur-file line-num)
        (dumb-jump-parse-ag-response rawresults cur-file line-num)))))

(defun dumb-jump-parse-grep-response (resp cur-file cur-line-num)
  "Takes a grep response RESP and parses into a list of plists"
  (let* ((resp-no-warnings (-filter (lambda (x)
                                      (and (not (s-starts-with? "grep:" x))
                                           (not (s-contains? "No such file or" x))))
                                    (s-split "\n" resp)))
         (parsed (butlast (-map (lambda (line) (s-split ":" line)) resp-no-warnings)))
         (results (-mapcat
                  (lambda (x)
                    (let* ((line-num (string-to-number (nth 1 x)))
                           (diff (- cur-line-num line-num)))
                      (list `(:path ,(nth 0 x) :line ,line-num :context ,(nth 2 x) :diff ,diff))))
                  parsed)))
    (-filter
     (lambda (x) (not (and
                       (string= (plist-get x :path) cur-file)
                       (= (plist-get x :line) cur-line-num))))
     results)))

(defun dumb-jump-parse-ag-response (resp cur-file cur-line-num)
  "Takes a ag response RESP and parses into a list of plists"
  (let* ((resp-lines (s-split "\n" resp))
         (parsed (butlast (-map (lambda (line) (s-split ":" line)) resp-lines)))
         (results (-mapcat
                  (lambda (x)
                    (let* ((line-num (string-to-number (nth 1 x)))
                           (diff (- cur-line-num line-num)))
                      (list `(:path ,(nth 0 x) :line ,line-num :context ,(nth 2 x) :diff ,diff))))
                  parsed)))
    (--filter
     (not (and
           (string= (plist-get it :path) cur-file)
           (= (plist-get it :line) cur-line-num)))
     results)))

(defun dumb-jump-re-match (re s)
  "Does regular expression RE match string S. If RE is nil return nil"
  (when (and re s)
    (s-match re s)))

(defun dumb-jump-get-ctx-type-by-language (lang pt-ctx)
  "Detect the type of context by the language"
  (let* ((contexts (--filter (string= (plist-get it ':language) lang) dumb-jump-language-contexts))
         (usable-ctxs
          (if (> (length contexts) 0)
              (-filter (lambda (ctx)
                         (and (or (null (plist-get ctx :left))
                                  (dumb-jump-re-match (plist-get ctx :left)
                                           (plist-get pt-ctx :left)))
                              (or (null (plist-get ctx :right))
                                  (dumb-jump-re-match (plist-get ctx :right)
                                      (plist-get pt-ctx :right)))))
                       contexts)
            nil))
         (use-ctx (= (length (--filter
                              (string= (plist-get it ':type)
                                       (and usable-ctxs (plist-get (car usable-ctxs) :type)))
                              usable-ctxs))
                     (length usable-ctxs))))

    ;(dumb-jump-message-prin1 "pt-ctx: %s | usable-ctxs: %s" pt-ctx usable-ctxs)
    (when (and usable-ctxs use-ctx)
      (plist-get (car usable-ctxs) :type))))

(defun dumb-jump-get-ext-includes (language)
  "Generate the --include grep argument of file extensions by LANGUAGE"
  (let ((exts (dumb-jump-get-file-exts-by-language language)))
    (dumb-jump-arg-joiner
     "--include"
     (-map
      (lambda (ext)
        (format "\\*.%s" ext))
      exts))))

(defun dumb-jump-arg-joiner (prefix values)
  "Helper to generate command arg with its PREFIX for each value in VALUES"
  (let ((args (s-join (format " %s " prefix) values)))
    (if (and args values)
      (format " %s %s " prefix args)
      "")))

(defun dumb-jump-get-contextual-regexes (lang ctx-type)
  "Get list of search regular expressions by LANG and CTX-TYPE (variable, function, etc)"
  (let* ((raw-rules
          (dumb-jump-get-rules-by-language lang))
         (ctx-type (if dumb-jump-ignore-context
                     nil
                     ctx-type))
         (ctx-rules
          (if ctx-type
              (-filter (lambda (r)
                         (string= (plist-get r :type)
                                   ctx-type))
                       raw-rules)
            raw-rules))
         (rules (if ctx-rules
                    ctx-rules
                  raw-rules))
         (regexes
          (-map
           (lambda (r)
             (plist-get r ':regex))
           rules)))
    ;(dumb-jump-message-prin1 "raw:%s\n ctx-ruls:%s\n rules:%s\n regexes:%s\n" raw-rules ctx-rules rules regexes)
    regexes))

(defun dumb-jump-populate-regexes (look-for regexes)
  (-map (lambda (x)
          (s-replace "JJJ" (regexp-quote look-for) x))
        regexes))

(defun dumb-jump-generate-ag-command (look-for cur-file proj regexes lang exclude-paths)
  "Generate the grep response based on the needle LOOK-FOR in the directory PROJ"
  (let* ((filled-regexes (dumb-jump-populate-regexes look-for regexes))
         (cmd (concat dumb-jump-ag-cmd " --nocolor --nogroup" (if (s-ends-with? ".gz" cur-file)
                                                            " --search-zip"
                                                          "")))
         (exclude-args (dumb-jump-arg-joiner "--ignore-dir" (-map (lambda (p) (s-replace proj "" p)) exclude-paths)))
         (regex-args (format "'%s'" (s-join "|" filled-regexes))))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd exclude-args regex-args proj))))

(defun dumb-jump-generate-grep-command (look-for cur-file proj regexes lang exclude-paths)
  "Generate the grep response based on the needle LOOK-FOR in the directory PROJ"
  (let* ((filled-regexes (-map (lambda (r) (format "'%s'" r))
                               (dumb-jump-populate-regexes look-for regexes)))
         (cmd (concat dumb-jump-grep-prefix " " (if (s-ends-with? ".gz" cur-file)
                                                    dumb-jump-zgrep-cmd
                                                  dumb-jump-grep-cmd)))
         (exclude-args (dumb-jump-arg-joiner "--exclude-dir" exclude-paths))
         (include-args (dumb-jump-get-ext-includes lang))
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (= (length regexes) 0)
        ""
        (dumb-jump-concat-command cmd dumb-jump-grep-args exclude-args include-args regex-args proj))))

(defun dumb-jump-concat-command (&rest parts)
  (s-join " " (-map #'s-trim
                    (--filter
                     (> (length it) 0)
                     parts))))

(defun dumb-jump-get-file-exts-by-language (language)
  "Get list of file extensions for a language"
  (-map (lambda (x) (plist-get x :ext))
        (-filter (lambda (x) (string= (plist-get x :language) language)) dumb-jump-language-file-exts)))

(defun dumb-jump-get-rules-by-language (language)
  "Get list of rules for a language"
  (let ((results (-filter (lambda (x) (string= (plist-get x ':language) language)) dumb-jump-find-rules)))
    (if dumb-jump-functions-only
        (-filter (lambda (x) (string= (plist-get x ':type) "function")) results)
      results)))

;;;###autoload
(define-minor-mode dumb-jump-mode
  "Minor mode for jumping to variable and function definitions"
  :global t
  :keymap dumb-jump-mode-map)

(provide 'dumb-jump)
;;; dumb-jump.el ends here
