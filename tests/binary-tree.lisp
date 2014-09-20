(fiasco:define-test-package :binary-tree-tests
  (:use :cl :binary-tree))
(in-package :binary-tree-tests)

(deftest member?-sanity-test ()
      (let ((bt (binary-tree 3 2 0 6 4)))
        (is (member? 4 bt))
        (is (not (member? 7 bt)))))
