(in-package #:cl-aristid)

(defconstant *BLACK* 0)
(defconstant *WHITE* 1)

(defun -> (old new)
  #'(lambda (seq)
    (substitute new old seq)))

(defun aristid (&key (angle 0) (len 0) (nodraw nil))
  #'(lambda (canvas)
      (dotimes (n len)
        (setf (canvas-point canvas)
              (magicl:.+ (canvas-point canvas) (canvas-dir canvas)))
        (if (null nodraw)
          (draw-point canvas)))
      (setf (canvas-dir canvas) (turn-angle (canvas-dir canvas) angle))
      canvas))

(defun flatten (tree)
  (loop for e in tree
        nconc
        (if (consp e)
            (copy-list e)
          (list e))))

(defun string-rewrite (str rules)
  (flatten
      (loop :for r :in rules :with seq := str
        :do (setq seq (funcall r seq))
        :finally (return seq))))

(defun commands (n str rules)
  (if (= n 0) (return-from commands str))
  (commands
    (1- n)
    (string-rewrite str rules)
    rules))

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

(defun apply-commands (canvas command-arr)
    (loop :for c :in command-arr :with seq := canvas
        :do (setq seq (funcall c seq))
        :finally (return seq)))

(defun draw (fractal canvas gen)
 (netpbm:write-to-file
    (format nil "~A_~3,'0d.pbm" (fractal-name fractal) gen)
    (draw-fractal fractal canvas gen)
    :format :pbm :encoding :ascii :if-exists :supersede))
