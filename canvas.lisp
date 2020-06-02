(in-package #:cl-aristid)

(defstruct (canvas (:constructor %make-canvas))
  matrix
  point
  dir)

(defun make-canvas (dims init-point)
  (let ((matrix (make-array dims :initial-element *WHITE*))
		(dir (magicl:from-list '(0 1) '(2 1) :type '(SINGLE-FLOAT)))
		(point (magicl:from-list init-point '(2 1) :type '(SINGLE-FLOAT))))
    (draw-point (%make-canvas :matrix matrix :point point :dir dir))))

(defun draw-point (canvas)
  (let ((x (round (magicl:tref (canvas-point canvas) 0 0)))
        (y (round (magicl:tref (canvas-point canvas) 1 0))))
    (if (or
          (>= x (array-dimension (canvas-matrix canvas) 0))
          (>= y (array-dimension (canvas-matrix canvas) 1))
          (< x 0)
          (< y 0))
        (return-from draw-point canvas))
    (setf (aref (canvas-matrix canvas) x y) *BLACK*))
    canvas)
