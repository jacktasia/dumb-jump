(require 'dumb-jump)
(require 'ert)

(ert-deftest dumb-jump-dummy-test ()
  (should (string= (dumb-jump-asdf) "asdf")))
