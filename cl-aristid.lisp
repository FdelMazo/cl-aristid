(in-package #:cl-aristid)

(setq *random-state* (make-random-state t))

(defun -> (old new &optional prob)
  #'(lambda (seq)
      (if (<= (random 1.00) prob)
          (substitute new old seq)
          seq)))

(defmacro defrule (sym -> replace &key (prob 1.00))
  `(-> ',sym ',replace ,prob))

(defun aristid (&key (angle 0) (len 0) (nodraw nil) (color ""))
  #'(lambda (canvas)
      (dotimes (n len)
        (setq canvas (canvas-move canvas))
        (if (null nodraw)
          (draw-point canvas color)))
      (setq canvas (turn-angle canvas angle))
      canvas))

(defmacro defaristid (name &rest body)
  `(defun ,name (canvas)
    (funcall (aristid ,@body) canvas)))

(defun [ (canvas)
  (setq canvas (push-stack canvas)))

(defun ] (canvas)
  (setq canvas (pop-stack canvas)))

(defun draw (fractal gen)
  (with-open-file (f (format nil "~A_~3,'0d.svg" (fractal-name fractal) gen)
                  :direction :output :if-exists :supersede)
    (cl-svg:stream-out f (draw-fractal fractal gen))))
