(defpackage :binary-tree
  (:use :cl :ordered :pfds-utils :qpq)
  (:import-from :anaphora
                :aif
                :it)
  (:import-from :closer-mop
                :compute-class-precedence-list)
  (:export #:node
           #:insert
           #:member?
           #:+empty-node+
           #:empty?
           #:new-node
           #:binary-tree
           #:tree-to-dotgraph
           #:element
           #:left-branch
           #:right-branch
           #:tree-depth
           #:tree-span))

(in-package :binary-tree)

;; Nodes

(defgeneric insert (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric %insert (element tree)
  (:documentation ""))

(defgeneric member? (element tree &optional candidate)
  (:documentation "Returns a new set with the element inside."))

(defgeneric insert (element tree)
  (:documentation "Return a new tree, with the element inserted. If the node already exists then return the same there."))



(defparameter +empty-node+ 'empty-node)

(defclass node ()
  ((element :initarg :element
            :initform (error 'argument-required :argument-name 'element)
            :reader element)
   (left-branch :initarg :left :initform 'empty-node :reader left-branch)
   (right-branch :initarg :right :initform 'empty-node :reader right-branch))
  (:metaclass contracted-class)
  (:invariants
   (lambda (obj)
     "Any given node is greater than each element in the left branch."
     (declare (debug 3))
     (implies (not (eql +empty-node+ (left-branch obj))) 
              (ord-lt (element (left-branch obj))
                      (element obj))))
   (lambda (obj)
     "Any given node is less than each element in the right branch."
     (declare (debug 3))
     (implies (not (eql +empty-node+ (right-branch obj))) 
               (ord-gt (element (right-branch obj))
                       (element obj))))))

(defmethod element ((object (eql +empty-node+)))
  +empty-node+)

(defmethod member? (element (node node) &optional candidate)
  (if (ord-leq element (element node))
      (member? element (left-branch node) (element node))
      (member? element (right-branch node) candidate)))


(defmethod insert (element (node node))
  (aif (%insert element node)
      it
    node))

(defmethod %insert (element (node node))
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
    ((ord-eql element (element node))
     nil)
    (t node)))

(defmethod empty? (node)
  nil)

(defmethod empty? ((node (eql +empty-node+)))
  t)

(defmethod member? (element (node (eql +empty-node+)) &optional candidate)
  (if candidate
      (ord-eql candidate element)
      nil))

(defmethod insert (element (node (eql +empty-node+)))
  (make-instance 'node :element element :left +empty-node+ :right +empty-node+))

;; Constructors & helpers

(defun new-node (value &key (left +empty-node+) (right +empty-node+))
  "Properly construct a new node."
  (unless (or (member (find-class 'node)
                      (compute-class-precedence-list (class-of left)))
              (eql left +empty-node+))
    (setf left (new-node left)))
  (unless  (or (member (find-class 'node)
                       (compute-class-precedence-list (class-of right)))
               (eql right +empty-node+))
    (setf right (new-node right)))

  (make-instance 'node
                 :element value
                 :left left
                 :right right))

(defun binary-tree (&rest values)
  "Eliminate duplicates. Sort the list. Take the median as the Root, and the
left branch the result of the recursively applying the same procedure on the
elements less than the mean and viceversa for the right branch. The base case
when the list is of length 3 or less."
  (let ((values (sort (remove-duplicates values) #'<)))
    (%binary-tree values)))

(defun %binary-tree  (&rest r-values)
  (let* ((r-values (apply #'append r-values)) ; Nested rest needs to be spliced
         (length (length r-values))
         (median (floor (/ length 2))))
    (cond ((eql length 3)
           (new-node (second r-values)
                     :left (first r-values)
                     :right (third r-values)))
          ((eql length 2)
           (new-node (second r-values)
                     :left (first r-values)))
          ((eql length 1)
           (new-node (first r-values)))
          (t (new-node (nth median r-values)
                       :left (%binary-tree
                              (subseq r-values 0 median))
                       :right (%binary-tree
                               (subseq r-values (1+ median)))) ))))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "NODE: ~A Left: ~A, Right: ~A"
            (element obj)
            (if (eql (class-of obj) (class-of (left-branch obj)))
                (element (left-branch obj))
                "Ø"
                )
            (if (eql (class-of obj) (class-of (right-branch obj)))
                (element (right-branch obj))
                "Ø"
                ))))

(defmethod print-object ((obj (eql +empty-node+)) stream)
  "In order to help checking the results"
  (format stream "Ø"))


;; Utils

#+(and sbcl swank)
(defmethod tree-to-dotgraph ((root node) &optional (output-file #P"~/tree.dot"))
  "Write graphivz's dot file for the tree."
  ;; If specify root to both branch. If node is not +empty-node+ recur.
  (let* ((output-emacs-file (namestring output-file))
         (output-img-file (format nil "~A-~A" output-emacs-file (gensym))))
    (with-open-file (output output-file
                            :direction :output
                            :if-exists :supersede)
      (format output "digraph{~%")
      (labels ((recur (node)
                 (with-accessors ((val element)
                                  (left left-branch)
                                  (right right-branch)) node
                   (let ((left-element (element left))
                         (right-element (element right)))
                     (when left-element
                       (format output "~A -> ~A~%" val left-element)
                       (recur left))
                     (when right-element
                       (format output "~A -> ~A~%" val right-element)
                       (recur right))))))
        (recur root))
      (format output "}"))
    (swank::eval-in-emacs
     `(progn
        (shell-command ,(format nil "dot ~A -Tpng > ~A"
                                output-emacs-file
                                output-img-file))
        (find-file ,output-img-file)
        t))))

(defun tree-depth (root)
  ""
  (cond ((empty? root) 0)
        (t (1+ (max (tree-depth (left-branch root))
                     (tree-depth (right-branch root)))))))

(defun tree-span (root)
  ""
  (labels ((follow-path (root accessor)
             (if (empty? (funcall accessor root))
                 0
                 (+ 1 (follow-path (funcall accessor root) accessor)))))
  (+ (follow-path root #'left-branch)
     (follow-path root #'right-branch))))
