;;;; cl-dragon.lisp

(in-package #:cl-dragon)

(defconstant *BLACK* 0)

;; TO DO: Remove setfs -> not functional
;; TO DO: Refactor entirely the commands function -> is a bottleneck, and its iterative
;; TO DO: Make draw recursive, instead of iterative -> kinda more functional

(defun d2r (degrees) (* pi (/ degrees 180.0)))

(defun rotation-matrix (angle)
  (magicl:from-list
    (list
      (cos (d2r angle))
      (- (sin (d2r angle)))
      (sin (d2r angle))
      (cos (d2r angle)))
    '(2 2) :type '(SINGLE-FLOAT)))

(defun turn-angle (dir angle)
  (magicl:@ (rotation-matrix angle) dir))

(defun draw-point (matrix point)
  (let ((x (round (magicl:tref point 0 0)))
        (y (round (magicl:tref point 1 0))))
    (if (or
          (>= x (second (magicl:shape matrix)))
          (>= y (first (magicl:shape matrix)))
          (< x 0)
          (< y 0))
        (return-from draw-point point))
    (setf (magicl:tref matrix x y) *BLACK*)))

(defun draw (matrix command-arr point)
  (let ((dir (magicl:from-list '(0 1) '(2 1) :type '(SINGLE-FLOAT))))
      (loop for command in command-arr do
        (case command
          (F  (dotimes (n 2)
            (draw-point matrix (setf point (magicl:.+ point dir)))))
          (RIGHT  (setf dir (turn-angle dir 90)))
          (LEFT   (setf dir (turn-angle dir -90)))
          (otherwise t)))))

(defun commands (n &optional (command-arr '(F A)))
  (if (= n 0) (return-from commands command-arr))
  (commands
    (1- n)
    (loop for item in command-arr append
      (case item
        (A '(A RIGHT B F RIGHT))
        (B '(LEFT F A LEFT B))
        (otherwise (list item))))))

(defun initial-matrix (dims init-point)
  (let ((matrix (magicl:ones dims :type '(SIGNED-BYTE 32))))
    (draw-point matrix init-point)
    matrix))

(defun dragon (dims init-point n)
  (let ((matrix (initial-matrix dims init-point))
        (command-arr (commands n)))
    (draw matrix command-arr init-point)
    (magicl:lisp-array matrix)))

(defun draw-dragon (n &optional (dims '(1000 1000))  (init-point (magicl:from-list '(500 500) '(2 1) :type '(SINGLE-FLOAT))))
 (netpbm:write-to-file
    (format nil "dragon_~3,'0d.pbm" n)
    (dragon dims init-point n) :format :pbm :encoding :binary :if-exists :supersede))
