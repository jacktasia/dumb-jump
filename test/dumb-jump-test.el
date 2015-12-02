;;; -*- lexical-binding: t -*-
(require 'dumb-jump)
(require 'ert)
(require 'f)

(setq test-data-dir (f-expand "./test/data"))

(ert-deftest dumb-jump-dummy-test ()
  (should (string= (dumb-jump-asdf) "asdf")))

(ert-deftest data-dir-exists-test ()
  (should (f-dir? test-data-dir)))
