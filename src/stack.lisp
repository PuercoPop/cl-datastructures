(in-package :stack)

;;; Interface

(defparameter +empty-stack+ 'empty-stack)

(defmethod print-object ((obj (eql +empty-stack+)) stream)
  "In order to help checking the results"
  (format stream "Ø"))

(defgeneric empty-p (stack)
  (:documentation "True if stack is empty."))

(defgeneric new-stack (head tail)
  (:documentation "Take two stacks and return a new stack."))

(defgeneric head (stack)
  (:documentation "Return the head of the stack, raise empty if stack is empty."))

(defgeneric tail (stack)
  (:documentation "Return the tail of the stack, raise empty if stack is empty."))

(defgeneric merge-stacks (left-stack right-stack)
  (:documentation "Return a new stack which contains the elements of both
  stacks."))

(defgeneric update (stack index value)
  (:documentation "Return a new stack which has the value at the index position
  substituting the old value."))


(define-condition empty-stack-condition (error)
  ((method-name :initarg :method-name :reader method-name))
  (:report
   (lambda (stream condition)
     (format stream "There is no ~A, only ZUUL" (method-name condition)))))

(define-condition invalid-subscript (error)
  ((text :initarg :text :reader :text)))

;;; Implementation

;;; Stack

(defclass stack ()
  ((head :initarg :head :initform +empty-stack+ :reader head)
   (tail :initarg :tail :initform +empty-stack+ :reader tail)))

(defmethod print-object ((obj stack) stream)
  "In order to help checking the results."
  (format stream "(~A, ~A)" (head obj) (tail obj)))


(defmethod empty-p ((stack stack))
  nil)

(defmethod new-stack (head tail)
  (make-instance 'stack :head head :tail tail))


(defmethod merge-stacks ((left-stack stack) (right-stack stack))
  (new-stack
   (head left-stack)
   (merge-stacks (tail left-stack) right-stack)))

(defmethod update ((stack stack) index value)
  (cond ((empty-p stack) (error 'invalid-subscript
                                :text "Out of bounds error. List not that long."))
        ((= 0 index) (new-stack value (tail stack)))
        (t (new-stack (head stack)
                      (update (tail stack) (- index 1) value)))))

(defmethod empty-p ((stack (eql +empty-stack+)))
  t)

(defmethod head ((stack (eql +empty-stack+)))
  (error 'empty-stack-condition :method 'head ))

(defmethod tail ((stack (eql +empty-stack+)))
  (error 'empty-stack-condition :method 'tail))

(defmethod merge-stacks ((left-stack (eql +empty-stack+)) (right-stack stack))
  right-stack)


;;; Exercise 2.1

(defun suffixes (stack)
  (cond ((empty-p stack) (list stack))
        (t
         (append (list stack) (suffixes (tail stack))))))



;;; Extra

(defmethod transverse-stack ((stack stack))
  (cond ((empty-p stack) (format t "~A~%" stack))
        (t (format t "~A~%" stack)
           (transverse-stack (tail stack)))))

(defmethod copy-stack ((stack stack))
  (cond ((empty-p stack) stack)
        (t (new-stack (head stack)
                      (copy-stack (tail stack))))))
