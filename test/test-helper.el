
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))
(require 'dumb-jump)
