(in-package :pfds)

(defvar *heap* (new-leftist-node 5))
(tree-to-dotgraph *heap*)
(insert 4 *heap*)
(tree-to-dotgraph *heap*)
