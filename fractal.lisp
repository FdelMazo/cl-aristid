(in-package #:cl-aristid)

(defstruct (fractal)
  name
  axiom
  rules)

(defun draw-fractal (fractal canvas gen)
  (let ((command-arr (commands gen (fractal-axiom fractal) (fractal-rules fractal))))
	(apply-commands canvas command-arr)
    (canvas-matrix canvas)))
