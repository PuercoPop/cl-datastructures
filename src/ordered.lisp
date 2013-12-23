(in-package :pfds)

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

(defmethod ord-neql (pattern candidate)
  (not (ord-eql pattern candidate)))

(defmethod ord-gt (pattern candidate)
  (not (ord-leq pattern candidate)))

;; Ordered for integers

(defmethod ord-eql ((pattern fixnum) (candidate fixnum))
  (eq pattern candidate))

(defmethod ord-lt ((pattern fixnum) (candidate fixnum))
  (< pattern candidate))

(defmethod ord-leq ((pattern fixnum) (candidate fixnum))
  (<= pattern candidate))
