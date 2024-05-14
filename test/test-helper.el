(require 'ert)
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:send-report t)
            (:report-file "/tmp/undercover-report.json"))
(require 'dumber-jump)

(require 'el-mock)
(eval-when-compile
  (require 'cl))

;;; test-helper.el ends here
