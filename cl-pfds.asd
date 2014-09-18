(in-package :cl-user)

(asdf:defsystem #:cl-pfds
  :name "Purely Functional Datasctructes"
  :description "Functional datastructures from the Okasaki book."
  :author "Javier Olaechea <pirata@gmail.com>"
  :version "20130401"
  :serial t
  :license "<3"
  :pathname "src/"
  :depends-on (#:anaphora
               #:quid-pro-quo)
               #:closer-mop
  :components ((:file "packages")
               (:file "utils")
               (:file "ordered")
               (:file "stack")
               (:file "binary-tree")
               (:file "leftist-heap")))

(asdf:defsystem #:cl-pfds-tests
  :depends-on (#:cl-pfds
               #:fiasco)
  :serial t
  :pathname "tests/"
  :components ((:file "packages")
               (:file "ordered")
               (:file "ch02-tests")
               (:file "binary-tree")
               (:file "runner")))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cl-pfds))))
  (asdf:load-system :cl-pfds-tests)
  (asdf/package:symbol-call :pfds-tests 'runner))
