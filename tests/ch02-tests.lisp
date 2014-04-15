(in-package :pfds-tests)

(def-suite ch02
    :description "Data structures from chapter two."
    )
(in-suite ch02)



;; (new-stack 3 (new-stack 1 (new-stack 2 (make-instance 'empty-stack))))
;; (trace suffixes)
;; (suffixes (new-stack 3 (new-stack 1 (new-stack 2 (make-instance 'empty-stack)))))
;; (new-stack 3 (new-stack 1 (new-stack 2 (make-instance 'empty-stack))))
;; (transverse-stack (new-stack 3 (new-stack 1 (new-stack 2 (make-instance 'stack)))))


;; (member? 3 (make-instance 'zet :element 3))
;; (member? 3 +empty-zet+)

;; (print-object (make-instance 'stack :head 1) t)
;; (print-object +empty-stack+ t)



;; (insert 3 (make-instance 'zet :element 3))
