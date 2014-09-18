(in-package :cl-user)

(defpackage :pfds-utils
  (:use :cl)
  (:export
   #:argument-required))

(defpackage :stack
  (:use :cl :qpq))

(defpackage :ordered
  (:use :cl)
  (:export #:ord-eql
           #:ord-lt
           #:ord-leq
           #:ord-neql
           #:ord-gt))

(defpackage :leftist-heap
  (:use :cl :binary-tree :ordered :qpq)
  (:export #:s-value
           #:merge-leftist-node
           #:find-min
           #:delete-min
           #:rank
           #:leftist-node
           #:+empty-heap+))

(defpackage :red-black-tree
  (:use :cl :binary-tree :ordered)
  (:export #:color
           #:balance
           #:+empty-node+))


(uiop/package:define-package :pfds
  (:use :cl :ordered :stack :binary-tree :leftist-heap :red-black-tree)
  (:reexport :ordered :stack :binary-tree :leftist-heap :red-black-tree))
