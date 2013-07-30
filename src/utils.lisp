(in-package :pfds)

(defun make-counter ()
  "Complete useless, SBCL has its own profiler and instrumentation."
  (let ((count 0))
    (lambda (operation)
      (ecase operation
        (:reset (setf count 0))
        (:add (incf count))
        (:show count)))))
