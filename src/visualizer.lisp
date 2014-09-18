(defpackage :ds-visualizer
  (:use :cl :binary-tree :sdl))

(in-package :ds-visualizer)

(defun translate-point (p1 &key (x 0) (y 0))
  (point :x (+ (x p1) x)
         :y (+ (y p1) y)))

(defun draw-node (element center)
  (draw-filled-circle-* (x center) (y center) 20
                        :color *red*)
  (draw-string-solid (format nil "~A" element) (translate-point center :x -5 :y -10)))

(defvar node-vertical-split 30)
(defvar node-horizontal-split 30)

(defun %node-horizontal-splits (node-levels current-split)
  "Return a list of the horizontal distance between parents and children from root to leaves."
  (cond
    ((eql node-levels 1) (cons current-split nil))
    (t (cons current-split
             (%node-horizontal-splits (decf node-levels)
                                      (+ (* 2 current-split)
                                         node-horizontal-split))))))

(defun draw-tree (tree)
  "First compute height and width, in levels of  nodes from the root..

Constants: node height and width separation, 70"
  (let* ((height-levels (tree-depth tree))
         (width-levels (tree-span tree))
         (left-margin 30)
         (right-margin 30)
         (top-margin 30)
         (bottom-margin 30)
         (node-horizontal-splits (reverse
                                 (%node-horizontal-splits
                                  width-levels
                                  node-vertical-split)))
         (window-height (+ top-margin bottom-margin
                           (* height-levels node-vertical-split)))
         (window-width (+ right-margin left-margin
                          (reduce #'+ node-horizontal-splits))
           )
         (root-position (point :x
                               800
                               ;; (/ (- window-width left-margin right-margin)
                               ;;       2)
                               :y top-margin)))
    (labels ((iter (node horizontal-offset vertical-offset depth)
               (draw-node (element node)
                          (translate-point root-position
                                           :x horizontal-offset
                                           :y vertical-offset))
               (unless (empty? (left-branch node))
                 (iter (left-branch node)
                       (- horizontal-offset
                          (elt node-horizontal-splits depth))
                       (+ vertical-offset
                          node-vertical-split)
                       (1+ depth)))
               (unless (empty? (right-branch node))
                 (iter (right-branch node)
                       (+ horizontal-offset
                          (elt node-horizontal-splits depth))
                       (+ vertical-offset
                          node-vertical-split)
                       (1+ depth)))))
      (with-init ()
        (window
         ;; window-width window-height
         1600 900
                :title-caption "Node Visualizer"
                :icon-caption "Node Visualizer")

        (initialise-default-font *font-10x20*)
        (with-events ()
          (:quit-event () t)
          (:idle ()
                 (iter tree 0 0 0)
                 (update-display)))))))
