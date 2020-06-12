(in-package #:cl-aristid)

(setf *random-state* (make-random-state t))

(defun -> (old new &optional prob)
  #'(lambda (seq)
      (if (<= (random 1.00) prob)
          (substitute new old seq)
          seq)))

(defmacro defrule (sym -> replace &key (prob 1.00))
  `(-> ',sym ',replace ,prob))

(defun aristid (&key (angle 0) (len 0) (nodraw nil) (color "black"))
  #'(lambda (canvas)
      (dotimes (n len)
        (setf canvas (canvas-move canvas))
        (if (null nodraw)
          (draw-point canvas color)))
      (setf canvas (turn-angle canvas angle))
      canvas))

(defun [ (canvas)
  (setf canvas (push-stack canvas))
  canvas)

(defun ] (canvas)
  (setf canvas (pop-stack canvas))
  canvas)

(defmacro defaristid (name &rest body)
  `(defun ,name (canvas)
    (funcall (aristid ,@body) canvas)))

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

(defun apply-commands (canvas command-arr)
    (loop :for c :in command-arr :with seq := canvas
        :do (if (fboundp c) (setq seq (funcall c seq)))
        :finally (return seq)))

(defun draw (fractal gen)
  (with-open-file (s (format nil "~A_~3,'0d.svg" (fractal-name fractal) gen)
                  :direction :output :if-exists :supersede)
    (cl-svg:stream-out s (draw-fractal fractal gen))))
