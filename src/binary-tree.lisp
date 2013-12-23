(in-package :pfds)


;; Nodes

(defgeneric insert (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric %insert (element tree)
  (:documentation ""))

(defgeneric member? (element tree &optional candidate)
  (:documentation "Returns a new set with the element inside."))

(defgeneric insert (element tree)
  (:documentation "Return a new tree, with the element inserted. If the node already exists then return the same there."))


(defclass node ()
  ((element :initarg :element :initform nil :reader element)
   (left-branch :initarg :left :initform nil :reader left-branch)
   (right-branch :initarg :right :initform nil :reader right-branch)))


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

;; Empty node is just an specialization of node.

(defparameter +empty-node+ (make-instance 'node))

(defmethod empty? ((node (eql +empty-node+)))
  t)

(defmethod member? (element (node (eql +empty-node+)) &optional candidate)
  (if candidate
      (ord-eql candidate element)
      nil))

(defmethod insert (element (node (eql +empty-node+)))
  (make-instance 'node :element element :left +empty-node+ :right +empty-node+))

;; Constructos & helpers

(defun new-node (value &key (left +empty-node+)
                         (right +empty-node+))
  "Properly construct a new node."

  (unless (typep left 'node)
    (setf left (new-node left)))
  (unless (typep right 'node)
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
    (labels ((r-helper (&rest r-values)
               (let* ((r-values (apply #'append r-values)) ; Nested rest needs to be spliced
                      (length (length r-values))
                      (median (floor (/ length 2))))
                 (cond ((= length 3)
                        (new-node (second r-values)
                                  :left (first r-values)
                                  :right (third r-values)))
                       ((= length 2)
                        (new-node (second r-values)
                                  :left (first r-values)))
                       ((= length 1)
                        (new-node (first r-values)))
                       (t (new-node (nth median r-values)
                                    :left (r-helper
                                           (subseq r-values 0 median))
                                    :right (r-helper
                                            (subseq r-values (1+ median)))) )))))
      (r-helper values))))

(defmethod print-object ((obj (eql +empty-node+)) stream)
  "In order to help checking the results"
  (format stream "Ø"))

;; (defmethod element ((node (eql +empty-node+)))
;;   "Ø")

#+sbcl
(defmethod tree-to-dotgraph ((root node) &optional (output-file #P"~/tree.dot"))
  "Write graphivz's dot file for the tree."
  ;; If specify root to both branch. If node is not +empty-nodenode+ recur.
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

;; (member 4 (binary-tree 3 2 0 6 4))
