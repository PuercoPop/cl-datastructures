(in-package :pfds)

(defgeneric less-than (first second)
  (:documentation "Returns t if the first object comes before the second one in a given order."))

(defgeneric more-than (first second)
  (:documentation "Returns t if the second object comes before the first one in a given order."))

(defgeneric insert (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric member? (element tree)
  (:documentation "Returns a new set with the element inside."))

(defgeneric insert (element tree)
  (:documentation "Return a new tree, with the element inserted. If the node already exists then return the same tere."))

;; Renaming in order to prevent collision with function in common lisp
(defclass zet ()
  ((elem :initarg :element :accessor element)
   (left-branch :initarg :left :initform nil :accessor left :type zet)
   (right-branch :initarg :right :initform nil :accessor right :type zet)))

(defmethod empty ((set zet))
  nil)

(defmethod member? (element (set zet))
  (with-slots (left-branch right-branch elem) set
    (cond
      ((equal elem element) t)
      ((less-than left-branch element) (member? element left-branch))
      ((more-than left-branch element) (member? element right-branch))
      (t nil))))

(defmethod less-than ((first zet) second)
  (with-slots (elem) first
    (< elem second)))

(defparameter +empty-zet+ (make-instance 'zet))

(defmethod empty ((set (eql +empty-zet+)))
  t)

(defmethod member? (element (set (eql +empty-zet+)))
  nil)

(defmethod less-than ((first null) second)
  nil)

(defmethod more-than ((first null) second )
  nil)

(defmethod insert (element (tree (eql +empty-zet+)))
  (make-instance 'zet :element element))



(defclass tree (zet)
  ())




(defmethod insert (element (tree zet))
  (cond ((less-than element (element tree))
         (make-instance 'zet
                        :left (insert element (left tree))
                        :element (element tree)
                        :right (right tree)))
        ((more-than element (element tree))
                  (make-instance 'zet
                        :left (left tree)
                        :element (element tree)
                        :right (insert element (right tree))))
        (t (element tree))))

