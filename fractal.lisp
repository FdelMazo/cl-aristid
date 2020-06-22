(in-package #:cl-aristid)

(defstruct (fractal)
  name
  axiom
  rules)

(defun flatten (tree)
  (loop :for e :in tree :append
        (if (consp e) (copy-list e)
          (list e))))

(defun string-rewrite (str rules)
  (flatten
      (loop :for r :in rules :with seq := str
        :do (setq seq (funcall r seq))
        :finally (return seq))))

(defun commands (n str rules)
  (if (= n 0) (return-from commands str))
  (commands (1- n) (string-rewrite str rules)  rules))

(defun apply-commands (canvas command-arr)
  (loop :for c :in command-arr :with seq := canvas
      :do (if (fboundp c) (setq seq (funcall c seq)))
      :finally (return seq)))

(defun draw-fractal (fractal gen)
  (let* ((command-arr (commands gen (fractal-axiom fractal)
                                   (fractal-rules fractal)))
         (canvas (make-canvas))
         (svg (create-svg (apply-commands canvas command-arr))))
    (svg-add-iteration svg canvas)
    svg))
