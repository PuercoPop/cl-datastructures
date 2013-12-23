(in-package :pfds)


(defgeneric is-empty (heap)
  (:documentation "Is the heap empty?"))

(defgeneric insert (value heap)
  (:documentation "insert the value on the heap and return the heap."))

(defgeneric merge-leftist-node (heap-a heap-b)
  (:documentation "Takes to heaps and returned the merged heap."))

(defgeneric find-min (heap)
  (:documentation "Returns the minium element of the heap, raises Empty exception if empty."))

(defgeneric delete-min (heap)
  (:documentation "Delete the minium element of the heap, raises Empty exception if empty."))


;; Implementaton

(defclass leftist-node (node)
  ((rank :initarg rank :initform 0 :reader rank)))

(defparameter +empty-heap+ (make-instance 'leftist-node)
  "The node to represent the empty leftist binary heap.")

(defmethod merge-leftist-node ((heap-a leftist-node) (heap-b (eql +empty-heap+)))
  heap-a)

(defmethod merge-leftist-node ((heap-a (eql +empty-heap+)) (heap-b leftist-node))
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

(defun new-leftist-node (value &key (left +empty-heap+)
                                 (right +empty-heap+))
  "Ensures that the node is initialized with the proper rank."
  (if (>= (rank left) (rank right))
      (make-instance 'leftist-node
                     :element value
                     :rank (1+ (rank right))
                     :left left
                     :right right)
      (make-instance 'leftist-node
                     :element value
                     :rank (1+ (rank left))
                     :left right
                     :right left)))

(defmethod find-min ((heap leftist-node))
  (element heap))

(defmethod insert (value (heap leftist-node))
  (merge-leftist-node (new-leftist-node value)
                      heap))

(defmethod delete-min ((heap leftist-node))
  (merge-leftist-node (left-branch heap)
                      (right-branch heap)))
