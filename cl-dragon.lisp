;;;; cl-dragon.lisp

(in-package #:cl-dragon)

(defconstant *BLACK* 0)
(defconstant *WHITE* 1)

;; TO DO: Make draw call a macro like apply-rules that calls every function in the function-array

(defun flatten (tree)
  (loop for e in tree
        nconc
        (if (consp e)
            (copy-list e)
          (list e))))

(defun -> (old new)
  #'(lambda (seq)
    (substitute new old seq)))

(defmacro apply-rules (rules axiom)
  `(flatten
    (let ((seq ,axiom))
         (let* ,(loop for rule in `,rules
                  collect `(seq (funcall ,rule seq)))
                seq))))

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
          (>= x (array-dimension matrix 0))
          (>= y (array-dimension matrix 1))
          (< x 0)
          (< y 0))
        (return-from draw-point point))
    (setf (aref matrix x y) *BLACK*)))

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
    (apply-rules ((-> 'B '(LEFT F A LEFT B))
                  (-> 'A '(A RIGHT B F RIGHT)))
                 command-arr)))

(defun initial-matrix (dims init-point)
  (let ((matrix (make-array dims :initial-element *WHITE*)))
    (draw-point matrix init-point)
    matrix))

(defun dragon (dims init-point n)
  (let ((matrix (initial-matrix dims init-point))
        (command-arr (commands n)))
    (draw matrix command-arr init-point)
    matrix))

(defun draw-dragon (n &optional (dims '(1000 1000))  (init-point (magicl:from-list '(500 500) '(2 1) :type '(SINGLE-FLOAT))))
 (netpbm:write-to-file
    (format nil "dragon_~3,'0d.pbm" n)
    (dragon dims init-point n) :format :pbm :encoding :binary :if-exists :supersede))
