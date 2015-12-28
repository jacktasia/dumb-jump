;;; dumb-jump.el --- Dumb jumping to declarations

;; Copyright (C) 2015 jack angers
;; Author: jack angers
;; Version: 1.0
;; Package-Requires: ((f "0.17.3") (s "1.9.0") (dash "2.9.0"))
;; Keywords: programming
;;; Commentary:

;; Uses `grep` to jump to delcartions via a list of regular expressions based on the major mode you are in.

;;; Code:
(require 'org)
(require 'f)
(require 's)
(require 'dash)

;; TODO: display options to user if more than one match
;; TODO: add rules for more languages
;; TODO: make dumb-jump-test-rules run on boot?
;; TODO: add warning message if a mode has ZERO language rules...
;; TODO: add "searching.." message with a warning if it's slow to exclude directories
;; TODO: prefix private functions with dj/ or simliar

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
    (:type "function" :language "javascript"
           :regex "function\\s*JJJ\\s*\\\("
           :tests ("function test()" "function test ()"))
    (:type "function" :language "javascript"
           :regex "\\s*JJJ\\s*=\\s*function\\s*\\\("
           :tests ("test = function()")))
  "List of regex patttern templates organized by language
and type to use for generating the grep command")

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

(defun dumb-jump-get-point-context (sentence func)
  (let* ((loc (s-index-of func sentence))
         (sd (s-replace func "" sentence))
         (left (substring sd (- loc 1) loc))
         (right (substring sd loc (+ loc 1))))
       (org-combine-plists (plist-put nil :left left)
                           (plist-put nil :right right))))

;; this should almost always take (buffer-file-name)
(defun dumb-jump-get-project-root (filepath)
  "Keep looking at the parent dir of FILEPATH until a
denoter file/dir is found then return that directory
If not found, then return dumb-jump-default-profile"
  (let ((test-path filepath)
        (proj-root nil))
    (while (and (null proj-root)
                (not (null test-path)))
      (setq test-path (f-dirname test-path))
      (unless (null test-path)
        (-each dumb-jump-project-denoters
          (lambda (denoter)
            (when (f-exists? (f-join test-path denoter))
              (setq proj-root test-path))))))
    (if (null proj-root)
      (f-long dumb-jump-default-project)
      proj-root)))

(defun dumb-jump-go ()
  "Go to the function/variable declaration for thing at point"
  (interactive)
  (let* ((proj-root (dumb-jump-get-project-root (buffer-file-name)))
         (look-for (thing-at-point 'symbol))
         (pt-ctx (dumb-jump-get-point-context
                  (thing-at-point 'sentence)
                  (thing-at-point 'symbol)))
         (results (dumb-jump-run-command major-mode look-for proj-root pt-ctx))
         (result-count (length results))
         (top-result (car results)))
    (cond
     ((and (not (listp results)) (s-blank? results))
      (message "Could not find rules for mode '%s'." major-mode))
     ((= result-count 1)
      (dumb-jump-goto-file-line (plist-get top-result :path) (plist-get top-result :line)))
     (t
      (message "Un-handled results: %s -> %s" (prin1-to-string (dumb-jump-generate-command major-mode look-for proj-root pt-ctx)) (prin1-to-string results))))))

(defun dumb-jump-goto-file-line (thefile theline)
  "Open THEFILE and go line THELINE"
  ;(message "Going to file '%s' line %s" thefile theline)
  (find-file thefile)
  (goto-char (point-min))
  (forward-line (- (string-to-number theline) 1)))

(defun dumb-jump-run-command (mode lookfor tosearch pt-ctx)
  "Run the grep command based on emacs MODE and
the needle LOOKFOR in the directory TOSEARCH"
  (let* ((cmd (dumb-jump-generate-command mode lookfor tosearch pt-ctx))
         (rawresults (shell-command-to-string cmd)))
    ; (message "Running cmd '%s'" cmd)
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

(defun dumb-jump-get-ctx-type-by-mode (mode pt-ctx)
  "Detect the type of context by the mode"
  (let* ((lang (car (dumb-jump-get-languages-by-mode mode))) ;; TODO: support all
         (contexts (-filter
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

(defun dumb-jump-generate-command (mode lookfor tosearch pt-ctx)
  "Generate the grep response based on emacs MODE and
the needle LOOKFOR in the directory TOSEARCH"
  (let* ((lang (car (dumb-jump-get-languages-by-mode mode))) ;; TODO: support all
         (raw-rules
          (dumb-jump-get-rules-by-mode mode))
         (ctx-type
          (dumb-jump-get-ctx-type-by-mode mode pt-ctx))
         (rules
          (if ctx-type
              (-filter (lambda (r)
                         (string= (plist-get r :type)
                                   ctx-type))
                       raw-rules)
            raw-rules))
         (regexes
          (-map
           (lambda (r)
             (format "'%s'" (plist-get r ':regex)))
           rules))
         (meat
          (s-join " -e " (-map
                          (lambda (x) (s-replace "JJJ" lookfor x))
                          regexes))))
    (if (= (length regexes) 0)
        ""
        (concat dumb-jump-grep-prefix " " dumb-jump-grep-args " -e " meat " " tosearch))))

(defun dumb-jump-get-rules-by-languages (languages)
  "Get a list of rules with a list of languages"
  (-mapcat (lambda (lang) (dumb-jump-get-rules-by-language lang)) languages))

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
