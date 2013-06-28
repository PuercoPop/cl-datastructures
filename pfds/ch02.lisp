(in-package :pfds)

;;; Interface

;; (defgeneric empty (stack)
;;   (:documentation "Return an empty stack."))

(defgeneric empty-p (stack)
  (:documentation "True if stack is empty."))

(defgeneric new-stack (head tail)
  (:documentation "Take two stacks and return a new stack."))

(defgeneric head (stack)
  (:documentation "Return the head of the stack, raise empty if stack is empty."))

(defgeneric tail (stack)
  (:documentation "Return the tail of the stack, raise empty if stack is empty."))

(defgeneric merge-stacks (left-stack right-stack)
  (:documentation "Return a new stack which contains the elements of both stacks."))

(defgeneric update (stack index value)
  (:documentation "Return a new stack which has the value at the index position substituting the old value."))


(define-condition empty-stack (error)
  ((text :initarg :text :reader :text)))

(define-condition invalid-subscript (error)
  ((text :initarg :text :reader :text)))

;;; Implementation

(defclass stack ()
  ((head :initarg :head :initform nil)
   (tail :initarg :tail :initform nil)))

(defmethod print-object ((obj stack) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (head tail) obj
      (format stream "<~A,~A>" head tail))))

(defmethod empty ((stack stack))
  '())

(defun empty ()
  "An alias basically"
  (make-instance 'stack))

(defmethod empty-p ((stack stack))
  (with-slots (head tail) stack
    (if (and (null head) (null tail))
        t
        nil)))

(defmethod new-stack (head (tail stack))
  (make-instance 'stack :head head :tail tail))

(defmethod head ((stack stack))
  (with-slots (head) stack
    (when (null head)
      (error 'empty-stack :text "There is no head, only ZUUL"))
    head))

(defmethod tail ((stack stack))
  (with-slots (tail) stack
    (when (null tail)
      (error 'empty-stack :text "There is no tail, only ZUUL"))
    tail))

(defmethod merge-stacks ((left-stack stack) (right-stack stack))
  (cond ((empty-p left-stack) right-stack)
        (t
         (new-stack
          (head left-stack)
          (merge-stacks (tail left-stack) right-stack)))))

(defmethod update ((stack stack) index value)
  (cond ((empty-p stack) (error 'invalid-subscript
                                :text "Out of bounds error. LList not that long."))
        ((= 0 index) (new-stack value stack))
        (t (new-stack (head stack)
                      (update (tail stack) (- index 1) value)))))


;;; Exercise 2.1

(defmethod suffixes ((stack stack))
  (cond ((empty-p stack) (new-stack stack (make-instance 'stack)))
        (t (new-stack stack (suffixes (tail stack))))))

(suffixes (new-stack 3 (new-stack 1 (new-stack 2 (make-instance 'stack)))))

(tree-equal (new-stack 1 (new-stack 2 (make-instance 'stack)))
            (new-stack 1 (new-stack 2 (make-instance 'stack))))

;; #<STACK <#<STACK <1,#1=#<STACK <2,#2=#<STACK <NIL,NIL> {1006A315B3}>> {1006A31603}>> {1006A31653}>,#<STACK <#1#,#<STACK <#2#,#<STACK <NIL,NIL> {1006A316A3}>> {1006A316E3}>> {1006A31723}>> {1006A31763}>
