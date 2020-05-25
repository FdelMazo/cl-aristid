;;;; cl-dragon.lisp

(in-package #:cl-dragon)

(setf WHITE 1)
(setf BLACK 0)

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
  (let ((matrix (make-array dims :initial-element WHITE))
        (x (car init-point))
        (y (cadr init-point)))
          (setf (aref matrix x y) BLACK)
          matrix))

(defun dragon (dims init-point n)
  (let ((matrix (initial-matrix dims init-point))
        (command-arr (commands n)))
          matrix))

(defun draw-dragon (n &optional (dims '(50 50))  (init-point '(25 25))) (
  netpbm:write-to-file (format nil "dragon_~3,'0d.pbm" n) (dragon dims init-point n) :format :pbm :encoding :ascii :if-exists :supersede))
