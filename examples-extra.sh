#!/usr/bin/sbcl --script
(load "~/.sbclrc")
(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload "cl-aristid")

;;; Koch curve
(defun F (canvas) (funcall (cl-aristid:aristid :len 2) canvas))
(defun LEFT (canvas) (funcall (cl-aristid:aristid :angle 90) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle -90) canvas))

(defparameter rule-1 (cl-aristid:-> 'F '(F RIGHT F LEFT F LEFT F RIGHT F)))

(defparameter axiom '(RIGHT F))

(defparameter fractal (cl-aristid:make-fractal :name "koch"
										:rules (list rule-1)
										:axiom axiom))

(defparameter canvas (cl-aristid:make-canvas '(200 100) '(15 85)))

(cl-aristid:draw fractal canvas 4)



;;; Sierpinski arrowhead curve
(defun A (canvas) (funcall (cl-aristid:aristid :len 10) canvas))
(defun B (canvas) (funcall (cl-aristid:aristid :len 10) canvas))
(defun LEFT (canvas) (funcall (cl-aristid:aristid :angle 60) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle -60) canvas))

(defparameter rule-1 (cl-aristid:-> 'A '(B LEFT A LEFT B)))
(defparameter rule-2 (cl-aristid:-> 'B '(A RIGHT B RIGHT A)))

(defparameter axiom '(A))

(defparameter fractal (cl-aristid:make-fractal :name "arrowhead"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(defparameter canvas (cl-aristid:make-canvas '(300 350) '(280 10)))

(cl-aristid:draw fractal canvas 5)



;;; Quadratic Gosper
(defun F (canvas) (funcall (cl-aristid:aristid :len 2) canvas))
(defun LEFT (canvas) (funcall (cl-aristid:aristid :angle 90) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle -90) canvas))

(defparameter rule-1 (cl-aristid:-> 'X '(X F X LEFT Y F LEFT Y F RIGHT F X RIGHT F X LEFT Y F LEFT Y F F X RIGHT Y F RIGHT F X F X Y F LEFT F X RIGHT Y F RIGHT F X F X RIGHT Y F LEFT F X Y F LEFT Y F LEFT F X RIGHT F X RIGHT Y F Y F LEFT)))
(defparameter rule-2 (cl-aristid:-> 'Y '(RIGHT F X F X LEFT Y F LEFT Y F RIGHT F X RIGHT F X Y F RIGHT F X LEFT Y F Y F LEFT F X LEFT Y F RIGHT F X Y F Y F LEFT F X LEFT Y F F X RIGHT F X RIGHT Y F LEFT Y F LEFT F X RIGHT F X RIGHT Y F Y)))

(defparameter axiom '(LEFT Y F))

(defparameter fractal (cl-aristid:make-fractal :name "quadratic"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(defparameter canvas (cl-aristid:make-canvas '(300 300) '(270 20)))

(cl-aristid:draw fractal canvas 3)



;;; Crystal
(defun F (canvas) (funcall (cl-aristid:aristid :len 2) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle -90) canvas))

(defparameter rule-1 (cl-aristid:-> 'F '(F F RIGHT F RIGHT RIGHT F RIGHT F)))

(defparameter axiom '(F RIGHT F RIGHT F RIGHT F))

(defparameter fractal (cl-aristid:make-fractal :name "crystal"
										:rules (list rule-1)
										:axiom axiom))

(defparameter canvas (cl-aristid:make-canvas '(530 530) '(20 20)))

(cl-aristid:draw fractal canvas 5)



;;; Von Koch Snowflake
(defun F (canvas) (funcall (cl-aristid:aristid :len 20) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle 60) canvas))
(defun LEFT (canvas) (funcall (cl-aristid:aristid :angle -60) canvas))

(defparameter rule-1 (cl-aristid:-> 'F '(F LEFT F RIGHT RIGHT F LEFT F)))

(defparameter axiom '(LEFT LEFT F RIGHT RIGHT F RIGHT RIGHT F))

(defparameter fractal (cl-aristid:make-fractal :name "snowflake"
										:rules (list rule-1)
										:axiom axiom))

(defparameter canvas (cl-aristid:make-canvas '(650 600) '(20 295)))

(cl-aristid:draw fractal canvas 3)



;;; Levy curve
(defun F (canvas) (funcall (cl-aristid:aristid :len 10) canvas))
(defun RIGHT (canvas) (funcall (cl-aristid:aristid :angle 45) canvas))
(defun LEFT (canvas) (funcall (cl-aristid:aristid :angle -45) canvas))

(defparameter rule-1 (cl-aristid:-> 'F '(LEFT F RIGHT RIGHT F LEFT)))

(defparameter axiom '(LEFT LEFT F))

(defparameter fractal (cl-aristid:make-fractal :name "levy"
										:rules (list rule-1)
										:axiom axiom))

(defparameter canvas (cl-aristid:make-canvas '(670 500) '(175 380)))

(cl-aristid:draw fractal canvas 10)