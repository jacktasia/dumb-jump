;;; dumb-jump.el --- Dumb jumping to declarations

;; Copyright (C) 2015 jack angers
;; Author: jack angers
;; Version: 1.0
;; Package-Requires: ((f "0.17.3") (s "1.9.0") (dash "2.9.0"))
;; Keywords: programming
;;; Commentary:

;; Uses `grep` to jump to the declaration for a variable/function under point via a set of regular expressions based on the current `major-mode`.

;;; Code:
(require 'org)
(require 'f)
(require 's)
(require 'dash)

;; TODO: if it's a variable context and the current file is in list then just jump to that?
;; TODO: don't use modes just use file extensions...?
;; TODO: add rules for more languages!
;; TODO: make dumb-jump-test-rules run on boot?
;; TODO: prefix private functions with dj/ or simliar
;; TODO: .dumb-jump settings file for excludes: also respect in .projectile see: https://github.com/bbatsov/projectile#ignoring-files
;; TODO: time search operation. if above N then have a helpful not about setting up excludes

;;LANG=C grep -REn --exclude-dir /Users/jack/code/react-canvas/node_modules --exclude-dir /Users/jack/code/react-canvas/bower_components --include \*.js --include \*.jsx --include \*.html  -e '\s*width\s*=\s*' -e 'function\s*width\s*\(' -e '\s*width\s*=\s*function\s*\(' /Users/jack/code/react-canvas

;; https://github.com/jacktasia/dotemacs24/commit/3972d4decbb09f7dff78feb7cbc5db5b6979b0eb

(defvar dumb-jump-grep-prefix "LANG=C grep" "Prefix to grep command. Seemingly makes it faster for pure text.")

(defvar dumb-jump-grep-args "-REn" "Grep command args Recursive, [e]xtended regexes, and show line numbers")

(defvar dumb-jump-find-rules
  '((:type "function" :language "elisp"
           :regex "\\\(defun\\s+JJJ\\s*" :tests ("(defun test (blah)"))
    (:type "variable" :language "elisp"
           :regex "\\\(defvar\\b\\s*JJJ\\b\\s*" :tests ("(defvar test "))
    (:type "variable" :language "elisp"
           :regex "\\\(setq\\b\\s*JJJ\\b\\s*" :tests ("(setq test 123)"))
    ;; javascript
    (:type "variable" :language "javascript"
           :regex "\\s*JJJ\\s*=\\s*" :tests ("test = 1234"))
    ;; TODO: below row is dependent on TODO in interactive
    ;; (:type "variable" :language "javascript"
    ;;        :regex "\\\(\\s*JJJ\\s*,?\\s*\\\)?" :tests ("(test)" "(test, blah)"))
    (:type "function" :language "javascript"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))
    (:type "function" :language "javascript"
           :regex "\\s*JJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()")))
  "List of regex patttern templates organized by language
and type to use for generating the grep command")

(defvar dumb-jump-language-file-exts
  '((:language "elisp" :ext "el")
    (:language "javascript" :ext "js")
    (:language "javascript" :ext "jsx")
    (:language "javascript" :ext "html"))
  "Mapping of programming lanaguage(s) to file extensions")

(defvar dumb-jump-language-modes
  '((:language "elisp" :mode "emacs-lisp-mode")
    (:language "javascript" :mode "js2-mode")
    (:language "javascript" :mode "js-mode")
    (:language "javascript" :mode "javascript-mode")
    (:language "javascript" :mode "web-mode"))
  "Mapping of programming lanaguage(s) to emacs major mode(s)")

(defvar dumb-jump-language-contexts
  '((:language "javascript" :type "function" :right "(" :left nil)
    (:language "javascript" :type "variable" :right "." :left nil)
    (:language "elisp" :type "variable" :right ")" :left " ")
    (:language "elisp" :type "function" :right " " :left "(")))

(defvar dumb-jump-project-denoters '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile")
  "Files and directories that signify a directory is a project root")

(defvar dumb-jump-default-project "~"
  "The default project to search for searching if a denoter is not found in parent of file")

(defun dumb-jump-test-rules ()
  "Test all the rules and return count ofthose that fail
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

(defun dumb-jump-get-point-context (line func)
  (let* ((loc (s-index-of func line))
         (func-len (length func))
         (sen-len (length line))
         (right-loc-start (+ loc func-len))
         (right-loc-end (+ right-loc-start 1))
         (left (substring line (- loc 1) loc))
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

(defun dumb-jump-handle-multiple-choices (look-for proj results)
  (let* ((prompt-text (dumb-jump-generate-prompt-text look-for proj results))
         (input (read-from-minibuffer prompt-text))
         (choice (dumb-jump-parse-input (length results) input)))
    (if choice
      (dumb-jump-result-go (nth (1- choice) results))
      (message "Sorry, that's an invalid choice."))))

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

(defun dumb-jump-go ()
  "Go to the function/variable declaration for thing at point"
  (interactive)
  (let* ((cur-file (buffer-file-name))
         (proj-info (dumb-jump-get-project-root cur-file))
         (proj-root (plist-get proj-info :root))
         (proj-config (plist-get proj-info :file))
         (look-for (thing-at-point 'symbol))
         (lang (car (dumb-jump-get-languages-by-mode major-mode))) ;; TODO: support all
         (pt-ctx (dumb-jump-get-point-context
                  (thing-at-point 'line)
                  (thing-at-point 'symbol)))
         (ctx-type
          (dumb-jump-get-ctx-type-by-language lang pt-ctx))
         (regexes (dumb-jump-get-contextual-regexes major-mode ctx-type))
         (include-args (dumb-jump-get-ext-includes lang))
         (exclude-args (if (s-ends-with? "/.dumbjump" proj-config)
                           (dumb-jump-read-exclusions proj-config)
                           ""))
         (results (dumb-jump-run-command look-for proj-root regexes include-args exclude-args))
         (result-count (length results))
         (top-result (car results)))

    (cond
     ((and (not (listp results)) (s-blank? results))
      (message "Could not find rules for mode '%s'." major-mode))
     ((= result-count 1)
      (dumb-jump-result-go top-result))
     ((> result-count 1)
      ;; multiple results so let the user pick from a list
      ;; unless the match is in the current file
      (let ((matched (dumb-jump-current-file-result cur-file results)))
        ;;(if (and (string= ctx-type "variable") matched)
        ;; TODO: this should check if there is ONLY 1 and/or go to the clest line number ABOVE
        (if matched
            (dumb-jump-result-go matched)
            (dumb-jump-handle-multiple-choices look-for proj-root results)))
     )
     ((= result-count 0)
      (message "'%s' declaration not found (%s %s)." look-for lang ctx-type))
     (t
      (message "Un-handled results: %s " (prin1-to-string results))))))

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
    (dumb-jump-arg-joiner "--exclude-dir" exclude-paths)))

(defun dumb-jump-result-go (result)
  (dumb-jump-goto-file-line (plist-get result :path) (plist-get result :line)))

(defun dumb-jump-goto-file-line (thefile theline)
  "Open THEFILE and go line THELINE"
  ;(message "Going to file '%s' line %s" thefile theline)
  (find-file thefile)
  (goto-char (point-min))
  (forward-line (- (string-to-number theline) 1)))

(defun dumb-jump-current-file-result (path results)
  "Is the PATH in the list of RESULTS"
  (let ((matched (-filter (lambda (r) (string= path (plist-get r :path))) results)))
    (car matched)))

(defun dumb-jump-run-command (look-for proj regexes include-args exclude-args)
  "Run the grep command based on emacs MODE and
the needle LOOKFOR in the directory TOSEARCH"
  (let* ((cmd (dumb-jump-generate-command look-for proj regexes include-args exclude-args))
         (rawresults (shell-command-to-string cmd)))
    ;(message "RUNNING CMD '%s'" cmd)
    (if (s-blank? cmd)
       nil
      (dumb-jump-parse-grep-response rawresults))))

(defun dumb-jump-parse-grep-response (resp)
  "Takes a grep response RESP and parses into a list of plists"
  (let ((parsed (butlast (-map (lambda (line) (s-split ":" line)) (s-split "\n" resp)))))
    (-mapcat
      (lambda (x)
        (let ((item '()))
          (setq item (plist-put item :path (nth 0 x)))
          (setq item (plist-put item :line (nth 1 x)))
          (setq item (plist-put item :context (nth 2 x)))
          (list item)))
      parsed)))

(defun dumb-jump-get-ctx-type-by-language (lang pt-ctx)
  "Detect the type of context by the mode"
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
            nil)))
    (when usable-ctxs
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

(defun dumb-jump-get-contextual-regexes (mode ctx-type)
  (let* ((raw-rules
          (dumb-jump-get-rules-by-mode mode))
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
    regexes))

(defun dumb-jump-generate-command (look-for proj regexes include-args exclude-args)
  "Generate the grep response based on emacs MODE and
the needle LOOK-FOR in the directory PROJ"
  (let* ((filled-regexes (-map (lambda (x) (s-replace "JJJ" look-for x))regexes))
         (regex-args (dumb-jump-arg-joiner "-e" filled-regexes)))
    (if (= (length regexes) 0)
        ""
        (concat dumb-jump-grep-prefix " " dumb-jump-grep-args exclude-args include-args regex-args  proj))))

(defun dumb-jump-get-rules-by-languages (languages)
  "Get a list of rules with a list of languages"
  (-mapcat (lambda (lang) (dumb-jump-get-rules-by-language lang)) languages))

(defun dumb-jump-get-file-exts-by-language (language)
  "Get list of file extensions for a language"
  (-map (lambda (x) (plist-get x :ext))
        (-filter (lambda (x) (string= (plist-get x :language) language)) dumb-jump-language-file-exts)))

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

(global-set-key (kbd "C-M-g") 'dumb-jump-go)

(provide 'dumb-jump)
;;; dumb-jump.el ends here
