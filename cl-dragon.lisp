;;;; cl-dragon.lisp

(in-package #:cl-dragon)

(setf WHITE 1)
(setf BLACK 0)

(defun turn-right (dir)
  (case dir
    ('(0 1) (return-from turn-right '(-1 0)))
    ('(-1 0) (return-from turn-right '(0 -1)))
    ('(0 -1) (return-from turn-right '(1 0)))
    ('(1 0) (return-from turn-right '(0 1)))))

(defun turn-left (dir)
  (case dir
    ('(0 1) (return-from turn-left '(1 0)))
    ('(1 0) (return-from turn-left '(0 -1)))
    ('(0 -1) (return-from turn-left '(-1 0)))
    ('(-1 0) (return-from turn-left '(0 1)))))

(defun draw-point (matrix point)
  (let ((x (first point))
        (y (second point)))
        (if (or (>= x (array-dimension matrix 0)) (>= y (array-dimension matrix 1)) (< x 0) (< y 0))
          (return-from draw-point nil))
        (setf (aref matrix x y) BLACK)))

(defun draw (matrix command-arr point)
  (let ((dir '(0 1)))
      (loop for command in command-arr do
        (case command
          (F  (dotimes (n 2) (draw-point matrix (map-into point #'+ point dir))))
          (RIGHT  (setf dir (turn-right dir)))
          (LEFT  (setf dir (turn-left dir)))
          (otherwise t)))))

(defun commands (n &optional (command-arr '(F A)))
  (cond ((= n 0) (return-from commands command-arr)))
  (commands
    (1- n)
    (loop for item in command-arr append
      (case item
        (A '(A RIGHT B F RIGHT))
        (B '(LEFT F A LEFT B))
        (otherwise (list item))))))

(defun initial-matrix (dims init-point)
  (let ((matrix (make-array dims :initial-element WHITE)))
        (draw-point matrix init-point)
          matrix))

(defun dragon (dims init-point n)
  (let ((matrix (initial-matrix dims init-point))
        (command-arr (commands n)))
          (draw matrix command-arr init-point)
          matrix))

(defun draw-dragon (n &optional (dims '(500 500))  (init-point '(100 100))) (
  netpbm:write-to-file (format nil "dragon_~3,'0d.pbm" n) (dragon dims init-point n) :format :pbm :encoding :ascii :if-exists :supersede))
