(defpackage :red-black-tree
  (:use :cl :binary-tree :ordered :qpq :optima)
  (:import-from :closer-mop :compute-class-precedence-list)
  (:export #:color
           #:balance
           #:+empty-node+))

(in-package :red-black-tree)

(defgeneric color (node)
  (:documentation "Return the color of the node."))

(defgeneric number-of-black-childs (node)
  (:documentation "Return the number of black childs."))

(defclass red-black-node (node) ()
  (:documentation "A Common ancestor for red and black nodes a like."))

(defclass red-node (node)
  ((number-of-black-childs
    :documentation "The amount of black children of the node."))
  (:metaclass contracted-class)
  (:invariants
   (lambda (obj)
     "No red node has a red child."
     (and (not (eql 'red (color (left-branch obj))))
          (not (eql 'red (color (right-branch obj)))))))
  (lambda (obj)
    "Every path from a block node to an empty node contains the same number of
black nodes."
    (eql (number-of-black-childs (left-branch obj))
         (number-of-black-childs (right-branch obj)))))

(defclass black-node (node) ())

(defparameter +empty-node+ 'black-node)

(defmethod color ((node red-node))
  'red)

(defmethod color ((node black-node))
  'black)

(defmethod color ((node (eql +empty-node+)))
  'black)

(defmethod member? (element (node (eql +empty-node+)))
  nil)

(defmethod member? (element (node node))
  (cond
    ((ord-lt element (element node)) (member? element (left-branch node)))
    ((ord-gt element (element node)) (member? element (right-branch node)))
    (t t)))

#|
insert x s = T B a y b
   where ins E = T R E x E
         ins s@(T color a y b) =
             if x < y then balance color (ins a) y b
             else if x > y then balance color a y (ins b)
             else s
         T _ a y b = ins s
|#

(defun ins (node)
  "helper for insert."
  (match node
    (() ...)
    (() ...)))

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

(defmethod number-of-black-childs ((node (eql +empty-node+)))
  1)

(defmethod number-of-black-childs ((node red-node))
  (+ (number-of-black-childs (left-branch node))
     (number-of-black-childs (right-branch node))))

(defmethod number-of-black-childs ((node black-node))
  (+ 1
     (number-of-black-childs (left-branch node))
     (number-of-black-childs (right-branch node))))

(defmethod number-of-black-childs :around ((node black-node))
  "Memoize result."
  (if (slot-boundp node 'number-of-black-childs)
      (slot-value node 'number-of-black-childs)
      (setf (slot-value node 'number-of-black-childs)
            (call-next-method))))


(defun new-red-node (element &key (left +empty-node+) (right +empty-node+))
  (unless (or (member (find-class 'node)
                      (compute-class-precedence-list (class-of left)))
              (eql left +empty-node+))
    (error "The :left value element is not a subclas of node."))

  (unless (or (member (find-class 'node)
                      (compute-class-precedence-list (class-of right)))
              (eql right +empty-node+))
    (error "The :right value element is not a subclas of node."))

  (make-instance 'red-node
                 :element element
                 :left left
                 :right right))

(defun new-black-node (element &key (left +empty-node+) (right +empty-node+))
  (unless (or (member (find-class 'node)
                      (compute-class-precedence-list (class-of left)))
              (eql left +empty-node+))
    (error "The :left value element is not a subclas of node."))

  (unless (or (member (find-class 'node)
                      (compute-class-precedence-list (class-of right)))
              (eql right +empty-node+))
    (error "The :right value element is not a subclas of node."))

  (make-instance 'black-node
                 :element element
                 :left left
                 :right right))

(defun balance (node)
  (match node
    ((or (black-node :left (red-node :left (red-node :left a
                                                      :element x
                                                      :right b)
                                      :element y
                                      :right c)
                      :element z
                      :right d)
         (black-node :left (red-node :left a
                                     :element x
                                     :right (red-node :left b
                                                      :element y
                                                      :right c))
                     :element z
                     :right d)
         (black-node :left a
                     :element x
                     :right (red-node :left (red-node :left b
                                                      :element y
                                                      :right c)
                                      :element z
                                      :right d))
         (black-node :left a
                     :element x
                     :right (red-node :left b
                                      :element y
                                      :right (red-node :left c
                                                       :element z
                                                       :right d))))
     (new-red-node y
                   :left (new-black-node x :left a
                                           :right b)
                   :right (new-black-node  z
                                           :left c
                                           :right d)))
    (otherwise node)))
