(in-package #:cl-aristid)

(defstruct (canvas (:constructor %make-canvas))
  matrix
  point
  prev-point
  dir
  dims)

(defun make-canvas ()
  (let ((matrix (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel :height 1000 :width 1000))
    (dir '(0 1))
    (point '(500 500)))
    (draw-point (%make-canvas :matrix matrix :point point :dir dir :dims '(1000 1000) :prev-point point))))

(defun draw-point (canvas)
  (let ((x (round (first (canvas-point canvas))))
        (y (round (second (canvas-point canvas)))))
    (if (or
          (>= x (first (canvas-dims canvas)))
          (>= y (second (canvas-dims canvas)))
          (< x 0)
          (< y 0))
        (return-from draw-point canvas))
    (cl-svg:draw (canvas-matrix canvas) (:line  :x1 (first (canvas-point canvas)) 
                                                :y1 (second (canvas-point canvas))
                                                :x2 (first (canvas-prev-point canvas))
                                                :y2 (second (canvas-prev-point canvas))
                                                :stroke "black"))
    canvas))

(defun canvas-move (canvas)
  (setf (canvas-prev-point canvas) (copy-list (canvas-point canvas)))
  (map-into (canvas-point canvas) #'+ (canvas-point canvas) (canvas-dir canvas))
  canvas)

(defun d2r (degrees) (* pi (/ degrees 180.0)))

(defun rotate-dir (dir angle)
  (list (- (* (first dir) (cos (d2r angle))) (* (second dir) (sin (d2r angle))))
        (+ (* (first dir) (sin (d2r angle))) (* (second dir) (cos (d2r angle))))))

(defun turn-angle (canvas angle)
  (setf (canvas-dir canvas) (rotate-dir (canvas-dir canvas) angle))
  canvas)