(in-package :pfds-tests)

(def-suite stacks
    :description "Stacks Chapter 2")
(in-suite stacks)

(test merge-stacks ()
      (is (pfds:merge-stacks (new-stack 2 (new-stack 1 +empty-stack+))
                             (new-stack 4 (new-stack 3 +empty-stack+)))
          (new-stack 2 (new-stack  1 (new-stack 4 (new-stack 3 +empty-stack+))))))


(test update ()
      (is
       (new-stack 1 (new-stack 10 (new-stack 10 +empty-stack+)))
       (pfds:update
        (new-stack 1 (new-stack 2 (new-stack 10 +empty-stack+)))
        1
        10)))

(test empty-p ()
      (is (not 
           (empty-p (tail (new-stack 1 (new-stack 2 (new-stack 10 'empty-stack)))))))
      (is (empty-p 'empty-stack)))
