(in-package :cl-user)

(uiop/package:define-package :pfds
  (:use :cl :ordered :stack :binary-tree :leftist-heap #+nil :red-black-tree)
  (:reexport :ordered :stack :binary-tree :leftist-heap #+nil :red-black-tree))
