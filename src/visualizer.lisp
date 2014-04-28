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

(defun draw-tree (tree)
  "First compute height and width, in levels of  nodes from the root..

Constants: node height and width separation, 70"
  (let* ((height-levels (tree-depth tree))
         (width-levels (tree-span tree))
         (left-margin 30)
         (right-margin 30)
         (top-margin 30)
         (bottom-margin 30)
         (node-vertical-split 30)
         (node-horizontal-split 30)
         (window-height (+ top-margin bottom-margin
                           (* height-levels node-vertical-split)))
         (window-width (+ right-margin left-margin
                          (* width-levels node-horizontal-split))
           )
         (root-position (point :x (/ (- window-width left-margin right-margin)
                                     2)
                               :y top-margin)))
    (labels ((iter (node horizontal-offset vertical-offset)
               (draw-node (element node) (point :x horizontal-offset
                                                :y vertical-offset))
               (unless (empty? (left-branch node))
                 (iter (left-branch node)
                       (- horizontal-offset
                          node-horizontal-split)
                       (+ vertical-offset
                          node-vertical-split)))
               (unless (empty? (right-branch node))
                 (iter (right-branch node)
                       (+ horizontal-offset
                          node-horizontal-split)
                       (+ vertical-offset
                          node-vertical-split)))))
      (with-init ()
        (window window-width window-height
                :title-caption "Node Visualizer"
                :icon-caption "Node Visualizer")

        (initialise-default-font *font-10x20*)
        (with-events ()
          (:quit-event () t)
          (:idle ()
                 (iter tree (x root-position) (y root-position))
                 (update-display)))))))
