(in-package :pfds)

;; Ordered

(defgeneric ord-eql (pattern candidate)
  (:documentation "Is pattern equal to candidate?"))

(defgeneric ord-lt (pattern candidate)
  (:documentation "Is the pattern less than candidate?"))

(defgeneric ord-leq (pattern candidate)
  (:documentation "Is the pattern less or equal than the candidate?"))

;; Ordered for integers

(defmethod ord-eql ((pattern fixnum) (candidate fixnum))
  (eq pattern candidate))

(defmethod ord-lt ((pattern fixnum) (candidate fixnum))
  (< pattern candidate))

(defmethod ord-leq ((pattern fixnum) (candidate fixnum))
  (<= pattern candidate))





;;; Nodes

(defgeneric insert (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric member? (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric insert (element tree)
  (:documentation "Return a new tree, with the element inserted. If the node already exists then return the same tere."))

(defclass node ()
  ((element :initarg :element :initform 0 :reader element)
   (left-branch :initarg :left :initform nil :reader left-branch)
   (right-branch :initarg :right :initform nil :reader right-branch)))

(defmethod empty? ((node node))
  nil)

(defmethod member? (element (node node))
  (cond
    ((less-than (element node) element) (member? element (left-branch node)))
    ((more-than (element node) element) (member? element (right-branch node)))
    (t t)))

(defmethod insert (element (node node))
  (cond
    ((less-than element (element node))
     (make-instance 'node
                    :left (insert element (left-branch node))
                    :element (element node)
                    :right (right-branch node)))
    ((more-than element (element node))
     (make-instance 'node
                    :left (left-branch node)
                    :element (element node)
                    :right (insert element (right-branch node))))
    (t node)))

(defparameter +empty-node+ (make-instance 'node))

(defmethod empty? ((node (eql +empty-node+)))
  t)

(defmethod member? (element (node (eql +empty-node+)))
  nil)

(defmethod insert (element (node (eql +empty-node+)))
  (make-instance 'node :element element :left +empty-node+ :right +empty-node+))
