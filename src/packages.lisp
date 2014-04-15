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

(defpackage :binary-tree
  (:use :cl :ordered :pfds-utils :qpq)
  (:import-from :anaphora
                :aif
                :it)
  (:export #:node
           #:insert
           #:member?
           #:+empty-node+
           #:empty?
           #:new-node
           #:binary-tree
           #:tree-to-dotgraph
           #:element
           #:left-branch
           #:right-branch))

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
