(in-package :leftist-heap)


(defgeneric merge-leftist-node (heap-a heap-b)
  (:documentation "Takes to heaps and returned the merged heap."))

(defgeneric find-min (heap)
  (:documentation "Returns the minium element of the heap, raises Empty exception if empty."))

(defgeneric delete-min (heap)
  (:documentation "Delete the minium element of the heap, raises Empty exception if empty."))

(defgeneric rank (node)
  (:documentation "The length of the rightmost path to an empty node."))

(defgeneric s-value (node)
  (:documentation "Calcuate the S-value of the node, which is the
  minimum distance to a leaf (empty-node)."))

(defgeneric check-invariants (heap)
  (:documentation "Check the invariants of data structures."))


;; Implementaton

(defclass leftist-node (node)
  ((rank :documentation "The distance to a childless node (empty node).")
   (s-value)))

(defparameter +empty-heap+ (make-instance 'leftist-node)
  "The node to represent the empty leftist binary heap.")

(define-condition empty-heap-conditon (error)
  ((text :initarg :text :reader :text)))

(defmethod empty? ((heap leftist-node))
  nil)

(defmethod empty? ((heap (eql +empty-heap+)))
  t)

(defmethod merge-leftist-node ((heap-a leftist-node)
                               (heap-b (eql +empty-heap+)))
  heap-a)

(defmethod merge-leftist-node ((heap-a (eql +empty-heap+))
                               (heap-b leftist-node))
  heap-b)

(defmethod merge-leftist-node ((heap-a leftist-node) (heap-b leftist-node))
  (if (ord-leq (element heap-a) (element heap-b))
      (new-leftist-node (element heap-a)
                        :left (left-branch heap-a)
                        :right (merge-leftist-node (right-branch heap-a)
                                                   heap-b))
      (new-leftist-node (element heap-b)
                        :left (left-branch heap-b)
                        :right (merge-leftist-node heap-a
                                                   (right-branch heap-b)))))

(defun new-leftist-node (value &key (left +empty-heap+) (right +empty-heap+))
  "Ensures the node with the higher rank is put on the left spine."
  (if (>= (rank left) (rank right))
      (make-instance 'leftist-node
                     :element value
                     :left left
                     :right right)
      (make-instance 'leftist-node
                     :element value
                     :left right
                     :right left)))

(defmethod find-min ((heap leftist-node))
  (element heap))

(defmethod find-min ((heap (eql +empty-heap+)))
  (error 'empty-stack-condition
         :text "An empty heap doesn't have a minimum value."))

(defmethod insert (value (heap leftist-node))
  (merge-leftist-node (new-leftist-node value)
                      heap))

(defmethod delete-min ((heap leftist-node))
  (merge-leftist-node (left-branch heap)
                      (right-branch heap)))

(defmethod delete-min ((heap (eql +empty-heap+)))
  (error 'empty-stack-condition
         :text "An empty stack has no element to delete."))

(defmethod print-object ((node leftist-node) stream)
  (print-unreadable-object (node stream :type t :identity nil)
    (princ (element node) stream)))


(defmethod rank ((node leftist-node))
  (1+ (rank (right-branch node))))

(defmethod rank ((node (eql +empty-heap+)))
  0)

(defmethod rank :around ((node leftist-node))
  (if (slot-boundp node 'rank)
      (slot-value node 'rank)
      (setf (slot-value node 'rank) (call-next-method))))

(defmethod s-value ((node leftist-node))
  (1+ (min (s-value (left-branch node))
           (s-value  (right-branch node)))))

(defmethod s-value ((node (eql +empty-heap+)))
  0)

(defmethod s-value :around ((node leftist-node))
  (if (slot-boundp node 's-value)
      (slot-value node 's-value)
      (setf (slot-value node 's-value) (call-next-method))))

(defmethod check-invariants ((node leftist-node))
  "The path to a leaf is shorter when transversing the right branch
And the values are increasing as the rank goes up."
  (and
   (<= (rank (right-branch node))
       (rank (left-branch node)))
   (check-invariants (right-branch node))
   (check-invariants (left-branch node))))

(defmethod check-invariants ((node (eql +empty-heap+)))
  t)

(defun heap-from-list (&rest values)
  "Eliminate duplicates, sort the list and insert by one the elements
  into a leftist heap."

  (let ((sorted-values (sort (remove-duplicates values) #'<)))
    (labels ((iter (result xs)
               (cond ((null xs) result)
                     (t (iter (insert (car xs) result)
                              (cdr xs))))))
      (if (= 1 (length sorted-values))
          (new-leftist-node (car sorted-values))
          (iter (new-leftist-node (car sorted-values)) (cdr sorted-values))))))
