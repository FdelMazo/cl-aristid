(in-package #:cl-aristid)

(defconstant *BLACK* 0)
(defconstant *WHITE* 1)

;; TO DO: Make draw call a macro like apply-rules that calls every function in the function-array

(defun make-canvas (dims point)
  (let ((matrix (make-array dims :initial-element *WHITE*))
    (dir (magicl:from-list '(0 1) '(2 1) :type '(SINGLE-FLOAT))))
    (%make-canvas :matrix matrix :point point :dir dir)))

(defstruct (canvas (:constructor %make-canvas))
  matrix
  point
  dir)

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

(defun draw (canvas command-arr)
  (let ((dir (magicl:from-list '(0 1) '(2 1) :type '(SINGLE-FLOAT)))
    (F (aristid :len 2)))
      (loop for command in command-arr do
        (case command
          (G (setf canvas (funcall F canvas)))
          (RIGHT  (setf (canvas-dir canvas) (turn-angle (canvas-dir canvas) 90)))
          (LEFT   (setf (canvas-dir canvas) (turn-angle (canvas-dir canvas) -90)))
          (otherwise t)))))

(defun commands (n &optional (command-arr '(G A)))
  (if (= n 0) (return-from commands command-arr))
  (commands
    (1- n)
    (apply-rules ((-> 'B '(LEFT G A LEFT B))
                  (-> 'A '(A RIGHT B G RIGHT)))
                 command-arr)))


(defun initial-canvas (dims init-point)
  (let ((canvas (make-canvas dims init-point)))
    (draw-point canvas)
    canvas))

(defun dragon (dims init-point n)
  (let ((canvas (initial-canvas dims init-point))
        (command-arr (commands n)))
    (draw canvas command-arr)
    (canvas-matrix canvas)))

(defun draw-dragon (n &optional (dims '(1000 1000))  (init-point (magicl:from-list '(500 500) '(2 1) :type '(SINGLE-FLOAT))))
 (netpbm:write-to-file
    (format nil "dragon_~3,'0d.pbm" n)
    (dragon dims init-point n) :format :pbm :encoding :binary :if-exists :supersede))


(defun aristid (&key (angle 0) (len 0) (draw t))
  #'(lambda (canvas)
      (dotimes (n len)
        (setf (canvas-point canvas) (magicl:.+ (canvas-point canvas) (canvas-dir canvas)))
        (if (eq draw t)
          (draw-point canvas)))
      canvas))
