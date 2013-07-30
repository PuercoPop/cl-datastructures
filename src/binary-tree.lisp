(in-package :pfds)

;; Ordered

(defgeneric ord-eql (pattern candidate)
  (:documentation "Is pattern equal to candidate?"))

(defgeneric ord-lt (pattern candidate)
  (:documentation "Is the pattern less than candidate?"))

(defgeneric ord-leq (pattern candidate)
  (:documentation "Is the pattern less or equal than the candidate?"))

;; Defining general methods on top of the specific ones.

(defmethod ord-neql (pattern candidate)
  (not (orld-eql pattern candidate)))

(defmethod ord-gt (pattern candidate)
  (not (orld-leq pattern candidate)))

;; Ordered for integers

(defmethod ord-eql ((pattern fixnum) (candidate fixnum))
  (eq pattern candidate))

(defmethod ord-lt ((pattern fixnum) (candidate fixnum))
  (< pattern candidate))

(defmethod ord-leq ((pattern fixnum) (candidate fixnum))
  (<= pattern candidate))



;; Nodes

(defgeneric insert (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric member? (element tree &optional candidate)
  (:documentation "Returns a new set with the element inside."))

(defgeneric insert (element tree)
  (:documentation "Return a new tree, with the element inserted. If the node already exists then return the same tere."))

(defclass node ()
  ((element :initarg :element :initform nil :reader element)
   (left-branch :initarg :left :initform nil :reader left-branch)
   (right-branch :initarg :right :initform nil :reader right-branch)))


(defmethod member? (element (node branch) &optional candidate)
  (cond
    ((ord-lt element (element node))
     (member? element (left-branch node) candidate))
    ((ord-gt element (element node))
     (member element (right-branch node)))
    (t t))) ; (t (member? element (right-branch node) node))

(defmethod insert (element (node branch))
  (cond
    ((ord-lt element (element node))
     (make-instance 'node
                    :left (insert element (left-branch node))
                    :element (element node)
                    :right (right-branch node)))
    ((ord-gt element (element node))
     (make-instance 'node
                    :left (left-branch node)
                    :element (element node)
                    :right (insert element (right-branch node))))
    (t node)))

(defmethod member? (element (node leaf) &optional candidate)
  (cond ((eql element (element node)) t)
        ((and (not (null candidate)) (eql element (element candidate))) t)
        (t nil)))

;; Empty node is just an specialization of node.

(defparameter +empty-node+ (make-instance 'node))

(defmethod empty? ((node (eql +empty-node+)))
  t)

(defmethod member? (element (node (eql +empty-node+)))
  nil)

(defmethod insert (element (node (eql +empty-node+)))
  (make-instance 'node :element element :left +empty-node+ :right +empty-node+))

(defmethod print-object ((obj (eql +empty-node+)) stream)
  "In order to help checking the results"
  (format stream "Ø"))
