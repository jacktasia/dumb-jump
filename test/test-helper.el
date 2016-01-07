
(require 'f)
(require 'undercover)
(defvar dumb-jump-file (f-join (f-parent (f-parent (f-this-file))) "dump-jump.el"))
(undercover dumb-jump-file)
(require 'dumb-jump)
