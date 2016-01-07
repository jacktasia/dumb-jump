(require 'f)

(defvar dumb-jump-test/test-path
  (f-parent (f-this-file)))

(defvar dumb-jump-test/root-path
  (f-parent dumb-jump-test/test-path))

(require 'ert)
(require 'undercover)
(undercover "dump-jump.el")
(require 'dumb-jump)
