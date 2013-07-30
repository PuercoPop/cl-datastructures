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

(defgeneric member? (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric insert (element tree)
  (:documentation "Return a new tree, with the element inserted. If the node already exists then return the same tere."))

(defclass node ()
  ((element :initarg :element :initform nil :reader element)
   (left-branch :initarg :left :initform nil :reader left-branch)
   (right-branch :initarg :right :initform nil :reader right-branch)))


(defmethod member? (element (node node))
  (cond
    ((ord-lt element (element node))
     (member? element (left-branch node) candidate))
    ((ord-gt element (element node))
     (member element (right-branch node)))
    (t t))) ; (t (member? element (right-branch node) node))

(defmethod insert (element (node node))
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

;; Empty node is just an specialization of node.

(defparameter +empty-node+ (make-instance 'node))

(defmethod empty? ((node (eql +empty-node+)))
  t)

(defmethod member? (element (node (eql +empty-node+)))
  nil)

(defmethod insert (element (node (eql +empty-node+)))
  (make-instance 'node :element element :left +empty-node+ :right +empty-node+))

;; Constructos & helpers

(defun new-node (value &key (left +empty-node+ left-p)
                            (right +empty-node+ right-p))
  "Create a new node, either a branch or a leaf."
  (if (or left-p right-p)
      (make-instance 'branch
                     :element value
                     :left left :right right)
      (make-instance 'leaf
                     :element value)))

(defun binary-tree (&rest values)
  "Eliminate duplicates. Sort the list. Take the median as the Root, and the
left branch the result of the recursively applying the same procedure on the
elements less than the mean and viceversa for the right branch. The base case
when the list is of length 3 or less."
  (sort (remove-duplicates values) #'<))

(defmethod print-object ((obj (eql +empty-node+)) stream)
  "In order to help checking the results"
  (format stream "Ø"))
