(in-package :pfds-tests)

(defun runner ()
  (fiasco:run-package-tests :package 'ordered-tests)
  (fiasco:run-package-tests :package 'stack-tests)
  (fiasco:run-package-tests :package 'binary-tree-tests))
