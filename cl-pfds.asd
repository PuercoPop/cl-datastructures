(in-package :cl-user)

(asdf:defsystem #:cl-pfds
  :name "Purely Functional Datasctructes"
  :description "Functional datastructures from the Okasaki book."
  :author "Javier Olaechea <pirata@gmail.com>"
  :version "20130401"
  :serial t
  :license "<3"
  :pathname "src/"
  :components ((:file "packages")
               (:file "stack")
               (:file "binary-tree")))
