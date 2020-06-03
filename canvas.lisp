(in-package #:cl-aristid)

(defstruct (canvas (:constructor %make-canvas))
  matrix
  point
  prev-point
  dir
  dims)

(defun make-canvas ()
  (let ((matrix (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel :height 1000 :width 1000))
    (dir (magicl:from-list '(0 1) '(2 1) :type '(SINGLE-FLOAT)))
    (point (magicl:from-list '(500 500) '(2 1) :type '(SINGLE-FLOAT))))
    (draw-point (%make-canvas :matrix matrix :point point :dir dir :dims '(1000 1000) :prev-point point))))

(defun draw-point (canvas)
  (let ((x (round (magicl:tref (canvas-point canvas) 0 0)))
        (y (round (magicl:tref (canvas-point canvas) 1 0))))
    (if (or
          (>= x (first (canvas-dims canvas)))
          (>= y (second (canvas-dims canvas)))
          (< x 0)
          (< y 0))
        (return-from draw-point canvas))
    (cl-svg:draw (canvas-matrix canvas) (:line :x1 (magicl:tref (canvas-point canvas) 0 0) 
                                                  :y1 (magicl:tref (canvas-point canvas) 1 0) 
                                                  :x2 (magicl:tref (canvas-prev-point canvas) 0 0) 
                                                  :y2 (magicl:tref (canvas-prev-point canvas) 1 0)
                                                  :stroke "black"))
    canvas))

(defun canvas-move (canvas)
  (setf (canvas-prev-point canvas) (canvas-point canvas))
  (setf (canvas-point canvas) (magicl:.+ (canvas-point canvas) (canvas-dir canvas)))
  canvas)