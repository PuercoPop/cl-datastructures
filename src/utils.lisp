(in-package :pfds)

(defun make-counter ()
  "Complete useless, SBCL has its own profiler and instrumentation."
  (let ((count 0))
    (lambda (operation)
      (ecase operation
        (:reset (setf count 0))
        (:add (incf count))
        (:show count)))))


(defun measure-funcalls (function &rest arguments)
  "Count how manytime a function is called. WIP."
  (sb-profile:reset)
  (sb-profile:profile function)
  (apply function arguments)
  (sb-profile:report)
  (sb-profile:unprofile function))
