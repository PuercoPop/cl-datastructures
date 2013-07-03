(in-package :pfds)

;;; Ordered

(defgeneric == (first second)
  (:documentation "t if first and second are equal, nil otherwise."))

(defgeneric less-than (first second)
  (:documentation "Returns t if the first object comes before the second one in a given order."))

(defgeneric more-than (first second)
  (:documentation "Returns t if the second object comes before the first one in a given order."))

(defmethod == ((first integer) (second integer))
  (eql first second))

(defmethod less-than ((first integer) (second integer))
  (< first second))

(defmethod more-than ((first integer) (second integer))
  (> first second))

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
