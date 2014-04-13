(in-package :pfds-tests)

(def-suite binary-tree
    :description "Binary tree tests.")
(in-suite binary-tree)

(test member? ()
      (let ((bt (binary-tree 3 2 0 6 4)))
        (is (member? 4 bt))
        (is (not (member? 7 bt)))))
