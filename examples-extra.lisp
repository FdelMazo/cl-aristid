(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload "cl-aristid")

;;; Koch curve
(cl-aristid:defaristid F :len 2)
(cl-aristid:defaristid LEFT :angle 90)
(cl-aristid:defaristid RIGHT :angle -90)

(defparameter rule-1 (cl-aristid:-> 'F '(F RIGHT F LEFT F LEFT F RIGHT F)))

(defparameter axiom '(RIGHT F))

(defparameter fractal (cl-aristid:make-fractal :name "koch"
										:rules (list rule-1)
										:axiom axiom))

(cl-aristid:draw fractal 4)


;;; Sierpinski arrowhead curve
(cl-aristid:defaristid A :len 5)
(cl-aristid:defaristid B :len 5)
(cl-aristid:defaristid LEFT :angle 60)
(cl-aristid:defaristid RIGHT :angle -60)

(defparameter rule-1 (cl-aristid:-> 'A '(B LEFT A LEFT B)))
(defparameter rule-2 (cl-aristid:-> 'B '(A RIGHT B RIGHT A)))

(defparameter axiom '(A))

(defparameter fractal (cl-aristid:make-fractal :name "arrowhead"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(cl-aristid:draw fractal 5)


;;; Quadratic Gosper
(cl-aristid:defaristid F :len 2)
(cl-aristid:defaristid LEFT :angle 90)
(cl-aristid:defaristid RIGHT :angle -90)

(defparameter rule-1 (cl-aristid:-> 'X '(X F X LEFT Y F LEFT Y F RIGHT F X RIGHT F X LEFT Y F LEFT Y F F X RIGHT Y F RIGHT F X F X Y F LEFT F X RIGHT Y F RIGHT F X F X RIGHT Y F LEFT F X Y F LEFT Y F LEFT F X RIGHT F X RIGHT Y F Y F LEFT)))
(defparameter rule-2 (cl-aristid:-> 'Y '(RIGHT F X F X LEFT Y F LEFT Y F RIGHT F X RIGHT F X Y F RIGHT F X LEFT Y F Y F LEFT F X LEFT Y F RIGHT F X Y F Y F LEFT F X LEFT Y F F X RIGHT F X RIGHT Y F LEFT Y F LEFT F X RIGHT F X RIGHT Y F Y)))

(defparameter axiom '(LEFT Y F))

(defparameter fractal (cl-aristid:make-fractal :name "quadratic"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(cl-aristid:draw fractal 3)


;;; Crystal
(cl-aristid:defaristid F :len 2)
(cl-aristid:defaristid RIGHT :angle -90)

(defparameter rule-1 (cl-aristid:-> 'F '(F F RIGHT F RIGHT RIGHT F RIGHT F)))

(defparameter axiom '(F RIGHT F RIGHT F RIGHT F))

(defparameter fractal (cl-aristid:make-fractal :name "crystal"
										:rules (list rule-1)
										:axiom axiom))

(cl-aristid:draw fractal 5)


;;; Von Koch Snowflake
(cl-aristid:defaristid F :len 5)
(cl-aristid:defaristid LEFT :angle 60)
(cl-aristid:defaristid RIGHT :angle -60)

(defparameter rule-1 (cl-aristid:-> 'F '(F LEFT F RIGHT RIGHT F LEFT F)))

(defparameter axiom '(LEFT LEFT F RIGHT RIGHT F RIGHT RIGHT F))

(defparameter fractal (cl-aristid:make-fractal :name "snowflake"
										:rules (list rule-1)
										:axiom axiom))

(cl-aristid:draw fractal 3)


;;; Levy curve
(cl-aristid:defaristid F :len 10)
(cl-aristid:defaristid LEFT :angle 45)
(cl-aristid:defaristid RIGHT :angle -45)

(defparameter rule-1 (cl-aristid:-> 'F '(LEFT F RIGHT RIGHT F LEFT)))

(defparameter axiom '(LEFT LEFT F))

(defparameter fractal (cl-aristid:make-fractal :name "levy"
										:rules (list rule-1)
										:axiom axiom))

(cl-aristid:draw fractal 10)

(quit)
