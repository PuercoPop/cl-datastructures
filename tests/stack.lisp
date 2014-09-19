(fiasco:define-test-package :stack-tests
  (:use :cl :stack))
(in-package :stack-tests)

(deftest stack-equal-sanity ()
  (is (stack-equal (new-stack 2 (new-stack 1 +empty-stack+))
                   (new-stack 2 (new-stack 1 +empty-stack+))))
  (is (not (stack-equal (new-stack 2 +empty-stack+)
                        (new-stack 2 (new-stack 1 +empty-stack+)))))
  (is (not (stack-equal (new-stack 2 (new-stack 1 +empty-stack+))
                        (new-stack 2 +empty-stack+)))) )

(deftest merge-stacks-sanity ()
  (is (merge-stacks (new-stack 2 (new-stack 1 +empty-stack+))
                    (new-stack 4 (new-stack 3 +empty-stack+))))
  (new-stack 2 (new-stack 1 (new-stack 4 (new-stack 3 +empty-stack+)))))


(deftest update-sanity ()
  (is (new-stack 1 (new-stack 10 (new-stack 10 +empty-stack+)))
      (update
       (new-stack 1 (new-stack 2 (new-stack 10 +empty-stack+)))
       1
       10)))

(deftest empty-p-sanity ()
  (is (not
       (empty-p (tail (new-stack 1 (new-stack 2 (new-stack 10 'empty-stack)))))))
  (is (empty-p +empty-stack+)))
