(require 'f)

(defvar dumb-jump-test/test-path
  (f-parent (f-this-file)))

(defvar dumb-jump-test/root-path
  (f-parent dumb-jump-test/test-path))

(require 'ert)
(require 'dumb-jump (f-expand "dumb-jump" dumb-jump-test/root-path))
