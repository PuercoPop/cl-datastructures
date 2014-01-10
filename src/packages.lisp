(in-package :cl-user)

(defpackage :pfds
  (:use :cl :anaphora)
  (:export #:merge-stacks
           #:update
           #:+empty-stack+
           #:new-stack
           #:make-counter
           #:member?
           #:new-node
           #:+empty-node+
           #:node
           #:element
           #:less-than
           #:more-than))

(defpackage :ordered
  (:use :cl)
  (:export
   #:ord-eql
   #:ord-lt
   #:ord-leq
   #:ord-neql
   #:ord-gt))

(defpackage :binary-tree
  (:use :cl)
  (:export
   #:node
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
  (:use :cl :binary-tree)
  (:export
   #:s-value
   #:merge-leftist-node
   #:find-min
   #:delete-min
   #:rank
   #:leftist-node
   #:+empty-heap+))

(defpackage :red-black-tree
  (:use :cl :binary-tree :ordered)
  (:export
   #:color
   #:balance
   #:+empty-node+))
