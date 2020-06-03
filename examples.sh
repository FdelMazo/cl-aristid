#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload "cl-aristid")

;;; We set up the fractal
;; We set up the aristids (i.e the drawing functions)
(defun F (canvas) (funcall (cl-aristid:aristid :len 2) canvas))
(defun LEFT (canvas) (funcall (cl-aristid:aristid :angle 90) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle -90) canvas))

;; We set up the production rules
(defparameter dragon-rule-1 (cl-aristid:-> 'A '(A RIGHT B F RIGHT)))
(defparameter dragon-rule-2 (cl-aristid:-> 'B '(LEFT F A LEFT B)))

;; We set up the fractal axiom
(defparameter dragon-axiom '(F A))

;; We create the fractal
(defparameter fractal (cl-aristid:make-fractal :name "dragon"
										:rules (list dragon-rule-1 dragon-rule-2)
										:axiom dragon-axiom))

;;; We set up the canvas
(defparameter canvas (cl-aristid:make-canvas '(100 100) '(25 25)))

;;; We draw the fractal
(cl-aristid:draw fractal canvas 10)
