(in-package :red-black-tree)

(defgeneric color (node)
  (:documentation "Return the color of the node."))

(defgeneric balance (color left element right)
  (:documentation "A helper to ensure the invariants are maintained."))

(defclass red-black-node (node) ()
  (:documentation "A Common ancestor for red and black nodes a
  like."))

(defclass red-node (node) ())

(defclass black-node (node) ())

(defparameter +empty-node+ (make-instance 'black-node))


(defmethod color ((node red-node))
  'red)

(defmethod color ((node black-node))
  'black)

(defmethod insert (element (node (eql +empty-node+)))
  (new-red-node element))

(defmethod insert (element (node red-black-node))
  (cond ((ord-lt element (element node)) (balance (color node)
                                                  _
                                                  (element node)
                                                  (right-branch node)))
        ((ord-gt element (element node)) (balance (color node)
                                                  (left-branch node)
                                                  (element node)
                                                  _))
        (t node)))

(defmethod balance)

(defmethod check-invariants ((node red-node))
  (and (eql 'red (color (left-branch node)))
       (eql 'red (color (right-branch node)))
       (check-invariants (left-branch node))
       (check-invariants (right-branch node))))
