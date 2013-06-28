(in-package :cl-user)

(asdf:defsystem #:cl-datastructures
  :name "michi"
  :description "Functional datastructures."
  :author "Javier Olaechea <pirata@gmail.com>"
  :version "20130401"
  :serial t
  :license "<3"
  :pathname "pfds/"
  :depends-on (#:optima)
  :components ((:file "packages")
               (:file "ch02")))
