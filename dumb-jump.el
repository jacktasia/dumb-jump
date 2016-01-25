;;; dumb-jump.el --- Dumb jumping to variable and function definitions

;; Copyright (C) 2015 jack angers
;; Author: jack angers
;; Version: 1.0
;; Package-Requires: ((f "0.17.3") (s "1.9.0") (dash "2.9.0"))
;; Keywords: programming
;;; Commentary:

;; **Dumb Jump** is an Emacs "jump to definition" package with support for multiple programming languages that favors "just working" over speed or accuracy.  This means minimal -- and ideally zero -- configuration with absolutely no stored indexes (TAGS) or persistent background processes.

;;; Code:
(require 'org)
(require 'f)
(require 's)
(require 'dash)


;; TODO: proper keybinding support
;; TODO: default excludes (.git..?) by language
;; TODO: support es6 javascript
;; TODO: rules should have (optional?) tests that fail :fails

;; TODO: support includes in `.dumbjump` -- offer to auto download for common languages/libraries
;; TODO: if it's not nil point context and there's no results then ask user if we should try all...
;; TODO: add more tests for rules for declarations in method signatures!
;; TODO: complete README add gif etc.
;; TODO: melpa recipe
;; TODO: track (point) and use to go
;; TODO: answer here: http://emacs.stackexchange.com/questions/10125/can-emacs-support-go-to-declaration-of-function-in-an-entire-project
;; TODO: make dumb-jump-test-rules run on boot?
;; TODO: prefix private functions with dj/ or simliar

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
  "LANG=C grep"
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
  t
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
    (:language "javascript" :ext "js")
    (:language "javascript" :ext "jsx")
    (:language "javascript" :ext "html")
    (:language "python" :ext "py")
    (:language "go" :ext "go"))
  "Mapping of programming lanaguage(s) to file extensions"
  :group 'dumb-jump)

(defcustom dumb-jump-language-contexts
  '((:language "javascript" :type "function" :right "(" :left nil)
    (:language "javascript" :type "variable" :right nil :left "(")
    (:language "javascript" :type "variable" :right ")" :left "(")
    (:language "javascript" :type "variable" :right "." :left nil)
    (:language "javascript" :type "variable" :right ";" :left nil)

    (:language "elisp" :type "function" :right " " :left " ")
    (:language "elisp" :type "function" :right " " :left "(")
    (:language "elisp" :type "variable" :right ")" :left " "))

  "List of under points contexts for each language. This helps limit
the number of regular expressions we use if we know that if there's a '('
immediately to the right of a symbol then it's probably a function call"
  :group 'dumb-jump)

(defcustom dumb-jump-project-denoters
  '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile")
  "Files and directories that signify a directory is a project root"
  :group 'dumb-jump)

(defcustom dumb-jump-default-project "~"
  "The default project to search for searching if a denoter is not found in parent of file"
  :group 'dumb-jump)

(defun message-prin1 (str &rest args)
  "Helper function when debuging applies prin1-to-string to all ARGS"
  (apply 'message str (-map 'prin1-to-string args)))

(defun dumb-jump-find-start-pos (line-in look-for cur-pos)
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
  (when (not dumb-jump-quiet)
    (apply 'message str args)))

(defun dumb-jump-get-point-context (line func cur-pos)
  (let* ((loc (dumb-jump-find-start-pos line func cur-pos))
         (func-len (length func))
         (sen-len (length line))
         (right-loc-start (+ loc func-len))
         (right-loc-end (+ right-loc-start 1))
         (left (substring line (max (- loc 1) 0) loc))
         (right (if (> right-loc-end sen-len)
                    ""
                  (substring line right-loc-start right-loc-end))))

       (org-combine-plists (plist-put nil :left left)
                           (plist-put nil :right right))))

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

;; this should almost always take (buffer-file-name)
(defun dumb-jump-get-project-root (filepath)
  "Keep looking at the parent dir of FILEPATH until a
denoter file/dir is found then return that directory
If not found, then return dumb-jump-default-profile"
  (let ((test-path filepath)
        (proj-file nil)
        (proj-root nil))
    (while (and (null proj-root)
                (not (null test-path)))
      (setq test-path (f-dirname test-path))
      (unless (null test-path)
        (-each dumb-jump-project-denoters
          (lambda (denoter)
            (when (f-exists? (f-join test-path denoter))
              (when (null proj-root)
                (setq proj-file denoter)
                (setq proj-root test-path)))))))
    (if (null proj-root)
      `(:root ,(f-long dumb-jump-default-project) :file nil)
      `(:root ,proj-root :file ,(f-join test-path proj-file)))))

(defun dumb-jump-get-language-by-filename (filename)
  "Get the programming language from the FILENAME"
  (let ((result (-filter
                 (lambda (f) (s-ends-with? (concat "." (plist-get f :ext)) filename))
                 dumb-jump-language-file-exts)))
    (if result
        (plist-get (car result) :language)
      (format ".%s file" (f-ext filename)))))

(defun dumb-jump-fetch-results ()
  (let* ((cur-file (buffer-file-name))
         (cur-line (thing-at-point 'line t))
         (cur-line-num (line-number-at-pos))
         (look-for (thing-at-point 'symbol t))
         (proj-info (dumb-jump-get-project-root cur-file))
         (proj-root (plist-get proj-info :root))
         (proj-config (plist-get proj-info :file))
         (lang (dumb-jump-get-language-by-filename cur-file))
;         (pt-ctx (dumb-jump-get-point-context cur-line look-for))
         (pt-ctx (if (not (string= cur-line look-for))
                     (dumb-jump-get-point-context cur-line look-for (current-column))
                 nil))
         (ctx-type
          (dumb-jump-get-ctx-type-by-language lang pt-ctx))
         (regexes (dumb-jump-get-contextual-regexes lang ctx-type))
         (include-args (dumb-jump-get-ext-includes lang))
         (exclude-args (if (s-ends-with? "/.dumbjump" proj-config)
                           (dumb-jump-read-exclusions proj-config)
                           ""))
         (raw-results (dumb-jump-run-command look-for proj-root regexes include-args exclude-args cur-file cur-line-num))
         (results (-map (lambda (r) (plist-put r :target look-for)) raw-results)))
    `(:results ,results :lang ,(if (null lang) "" lang) :symbol ,look-for :ctx-type ,(if (null ctx-type) "" ctx-type) :file ,cur-file :root ,proj-root)))

(defun dumb-jump-back ()
  (interactive)
  (if dumb-jump-last-location
    (let* ((last-loc (car dumb-jump-last-location))
          (path (plist-get last-loc :path)))
      (dumb-jump-message "Jumping back to%s line %s"
               (if (not (string= path (buffer-file-name)))
                   (concat " " (f-filename path))
                 "")
               (number-to-string (plist-get last-loc :line)))
      (setq dumb-jump-last-location (cdr dumb-jump-last-location))
      (dumb-jump-goto-file-point
       (plist-get last-loc :path)
       (plist-get last-loc :point)))
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
    ; (message-prin1 "lang:%s type:%s results: %s" lang ctx-type results)
    (cond
;     ((and (not (listp results)) (s-blank? results))
     ((> fetch-time dumb-jump-max-find-time)
      (dumb-jump-message "Took over %ss to find '%s'. Please add a .dumbjump file to '%s' with path exclusions"
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
  (let* ((match-sorted (-sort (lambda (x y) (< (plist-get x :diff) (plist-get y :diff))) results))
        ; moves current file results to the front of the list
        (match-cur-file-front (-concat
                               (-filter (lambda (x) (and (> (plist-get x :diff) 0)
                                                         (string= (plist-get x :path) cur-file))) match-sorted)

                               (-filter (lambda (x) (and (<= (plist-get x :diff) 0)
                                                         (string= (plist-get x :path) cur-file))) match-sorted)

                               (-filter (lambda (x) (not (string= (plist-get x :path) cur-file))) match-sorted)))

        (matches (dumb-jump-current-file-results cur-file match-cur-file-front))
        (var-to-jump (car matches))
        ;; TODO: handle if ctx-type is null but ALL results are variable
        (do-var-jump (and (or (= (length matches) 1) (string= ctx-type "variable") (string= ctx-type "")) var-to-jump)))
    ;(message-prin1 "type: %s | jump? %s | matches: %s | sorted: %s | results: %s" ctx-type var-to-jump matches match-sorted results)
    (if do-var-jump
        (dumb-jump-result-follow var-to-jump)
      (dumb-jump-prompt-user-for-choice look-for proj-root match-cur-file-front))))

(defun dumb-jump-read-exclusions (config-file)
  (let* ((root (f-dirname config-file))
         (contents (f-read-text config-file))
         (lines (s-split "\n" contents))
         (exclude-lines (-filter (lambda (f) (s-starts-with? "-" f)) lines))
         (exclude-paths (-map (lambda (f)
                                 (let* ((dir (substring f 1))
                                       (use-dir (if (s-starts-with? "/" dir)
                                                    (substring dir 1)
                                                    dir)))
                                   (f-join root use-dir)))
                               exclude-lines)))
    (if exclude-lines
      (dumb-jump-arg-joiner "--exclude-dir" exclude-paths)
      "")))

(defun dumb-jump-result-follow (result)
  (let ((pos (s-index-of (plist-get result :target) (plist-get result :context)))
        (thef (plist-get result :path))
        (line (plist-get result :line)))
    (when thef
      (add-to-list 'dumb-jump-last-location `(:line ,(line-number-at-pos) :path ,(buffer-file-name) :pos ,pos :point ,(point)))
      (dumb-jump-goto-file-line thef line pos))))

(defun dumb-jump-goto-file-line (thefile theline pos)
  "Open THEFILE and go line THELINE"
  ;(dumb-jump-message "Going to file '%s' line %s" thefile theline)
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

(defun dumb-jump-run-command (look-for proj regexes include-args exclude-args cur-file line-num)
  "Run the grep command based on the needle LOOKFOR in the directory TOSEARCH"
  (let* ((cmd (dumb-jump-generate-command look-for proj regexes include-args exclude-args))
         (rawresults (shell-command-to-string cmd)))
    ;(message-prin1 "RUNNING CMD '%s' RESULTS: %s" cmd rawresults)
    (if (s-blank? cmd)
       nil
      (dumb-jump-parse-grep-response rawresults cur-file line-num))))

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

(defun dumb-jump-get-ctx-type-by-language (lang pt-ctx)
  "Detect the type of context by the language"
  (let* ((contexts (-filter
                    (lambda (x) (string= (plist-get x ':language) lang))
                    dumb-jump-language-contexts))
         (usable-ctxs
          (if (> (length contexts) 0)
              (-filter (lambda (ctx)
                         (or (string= (plist-get ctx :left)
                                      (plist-get pt-ctx :left))
                             (string= (plist-get ctx :right)
                                      (plist-get pt-ctx :right))))
                       contexts)
            nil))
         (use-ctx (= (length (-filter
                              (lambda (x) (string= (plist-get x ':type)
                                                   (and usable-ctxs (plist-get (car usable-ctxs) :type))))
                              usable-ctxs))
                     (length usable-ctxs))))

    ;(message-prin1 "pt-ctx: %s | usable-ctxs: %s" pt-ctx usable-ctxs)
    (when (and usable-ctxs use-ctx)
      (plist-get (car usable-ctxs) :type))))

(defun dumb-jump-get-ext-includes (language)
  (let ((exts (dumb-jump-get-file-exts-by-language language)))
    (dumb-jump-arg-joiner
     "--include"
     (-map
      (lambda (ext)
        (format "\\*.%s" ext))
      exts))))

(defun dumb-jump-arg-joiner (prefix values)
  (let ((args (s-join (format " %s " prefix) values)))
    (if args
      (format " %s %s " prefix args)
      "")))

(defun dumb-jump-get-contextual-regexes (lang ctx-type)
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
             (format "'%s'" (plist-get r ':regex)))
           rules)))
    ;(message-prin1 "raw:%s\n ctx-ruls:%s\n rules:%s\n regexes:%s\n" raw-rules ctx-rules rules regexes)
    regexes))

(defun dumb-jump-generate-command (look-for proj regexes include-args exclude-args)
  "Generate the grep response based on the needle LOOK-FOR in the directory PROJ"
  (let* ((filled-regexes (-map (lambda (x) (s-replace "JJJ" look-for x)) regexes))
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (= (length regexes) 0)
        ""
        (concat dumb-jump-grep-prefix " " dumb-jump-grep-args exclude-args include-args regex-args proj))))

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
