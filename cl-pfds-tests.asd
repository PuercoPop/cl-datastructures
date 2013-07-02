(in-package :cl-user)

(asdf:defsystem :cl-pfds-tests
  :depends-on (cl-pfds fiveam flexi-streams)
  :serial t
  :pathname "tests/"
  :components ((:file "packages")
               (:file "ch02-tests")))

