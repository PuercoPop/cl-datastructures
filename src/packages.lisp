(in-package :cl)

(defpackage :pfds
  (:use :cl)
  (:export
   #:merge-stacks
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
