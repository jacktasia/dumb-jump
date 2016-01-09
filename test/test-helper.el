
(require 'ert)
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:send-report t)
            (:report-file "/tmp/undercover-report.json"))
(require 'dumb-jump)

(provide 'test-helper)
;;; test-helper.el ends here
