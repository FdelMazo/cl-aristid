(in-package #:cl-aristid)

(defstruct (fractal)
  name
  axiom
  rules)

(defun draw-fractal (fractal gen)
  (let ((command-arr (commands gen (fractal-axiom fractal) (fractal-rules fractal))))
    (create-svg (apply-commands (make-canvas) command-arr))))
