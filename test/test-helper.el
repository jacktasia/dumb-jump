
(require 'ert)
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:send-report t)
            (:report-file "/tmp/undercover-report.json"))
(require 'dumb-jump)

(require 'el-mock)
(eval-when-compile
  (require 'cl-lib))

;;; test-helper.el ends here
