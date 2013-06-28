(merge-stacks  (new-stack 2 (new-stack 1 (make-instance 'stack)))
               (new-stack 4 (new-stack 3 (make-instance 'stack))))

(update (new-stack 1 (new-stack 2 (new-stack 10 (make-instance 'stack))))
        2
        10)
