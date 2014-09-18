(defpackage :ordered
  (:use :cl)
  (:export #:ord-eql
           #:ord-lt
           #:ord-leq
           #:ord-neql
           #:ord-gt))

(in-package :ordered)

;; So as to not make all the datastructures depend on integers a
;; mininal interface, Ordered, is used, as show in page 14.

;; Ordered

(defgeneric ord-eql (pattern candidate)
  (:documentation "Is pattern equal to candidate?"))

(defgeneric ord-lt (pattern candidate)
  (:documentation "Is the pattern less than candidate?"))

(defgeneric ord-leq (pattern candidate)
  (:documentation "Is the pattern less or equal than the candidate?"))

;; Defining general methods on top of the specific ones.

(defgeneric ord-neql (pattern candidate)
  (:documentation "Is pattern not equal to candidate?")
  (:method (pattern candidate)
    (not (ord-eql pattern candidate))))

(defgeneric ord-gt (pattern candidate)
  (:documentation "Is the pattern greater than the candidate")
  (:method (pattern candidate)
    (not (ord-leq pattern candidate))))

;; Ordered for integers

(defmethod ord-eql ((pattern fixnum) (candidate fixnum))
  (eq pattern candidate))

(defmethod ord-lt ((pattern fixnum) (candidate fixnum))
  (< pattern candidate))

(defmethod ord-leq ((pattern fixnum) (candidate fixnum))
  (<= pattern candidate))
