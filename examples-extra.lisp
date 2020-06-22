(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload "cl-aristid")
(use-package 'cl-aristid)

;;; Koch curve
(defaristid F :len 2)
(defaristid LEFT :angle 90)
(defaristid RIGHT :angle -90)

(defparameter rule-1 (defrule F -> (F RIGHT F LEFT F LEFT F RIGHT F)))

(defparameter axiom '(RIGHT F))

(defparameter fractal (make-fractal :name "koch"
										:rules (list rule-1)
										:axiom axiom))

(draw fractal 4)


;;; Sierpinski arrowhead curve
(defaristid A :len 5)
(defaristid B :len 5)
(defaristid LEFT :angle 60)
(defaristid RIGHT :angle -60)

(defparameter rule-1 (defrule A -> (B LEFT A LEFT B)))
(defparameter rule-2 (defrule B -> (A RIGHT B RIGHT A)))

(defparameter axiom '(A))

(defparameter fractal (make-fractal :name "arrowhead"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(draw fractal 5)


;;; Quadratic Gosper
(defaristid F :len 2)
(defaristid LEFT :angle 90)
(defaristid RIGHT :angle -90)

(defparameter rule-1 (defrule X -> (X F X LEFT Y F LEFT Y F RIGHT F X RIGHT F X LEFT Y F LEFT Y F F X RIGHT Y F RIGHT F X F X Y F LEFT F X RIGHT Y F RIGHT F X F X RIGHT Y F LEFT F X Y F LEFT Y F LEFT F X RIGHT F X RIGHT Y F Y F LEFT)))
(defparameter rule-2 (defrule Y -> (RIGHT F X F X LEFT Y F LEFT Y F RIGHT F X RIGHT F X Y F RIGHT F X LEFT Y F Y F LEFT F X LEFT Y F RIGHT F X Y F Y F LEFT F X LEFT Y F F X RIGHT F X RIGHT Y F LEFT Y F LEFT F X RIGHT F X RIGHT Y F Y)))

(defparameter axiom '(LEFT Y F))

(defparameter fractal (make-fractal :name "quadratic"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(draw fractal 3)


;;; Crystal
(defaristid F :len 2)
(defaristid RIGHT :angle -90)

(defparameter rule-1 (defrule F -> (F F RIGHT F RIGHT RIGHT F RIGHT F)))

(defparameter axiom '(F RIGHT F RIGHT F RIGHT F))

(defparameter fractal (make-fractal :name "crystal"
										:rules (list rule-1)
										:axiom axiom))

(draw fractal 5)


;;; Von Koch Snowflake
(defaristid F :len 5)
(defaristid LEFT :angle 60)
(defaristid RIGHT :angle -60)

(defparameter rule-1 (defrule F -> (F LEFT F RIGHT RIGHT F LEFT F)))

(defparameter axiom '(LEFT LEFT F RIGHT RIGHT F RIGHT RIGHT F))

(defparameter fractal (make-fractal :name "snowflake"
										:rules (list rule-1)
										:axiom axiom))

(draw fractal 3)


;;; Levy curve
(defaristid F :len 10)
(defaristid LEFT :angle 45)
(defaristid RIGHT :angle -45)

(defparameter rule-1 (defrule F -> (LEFT F RIGHT RIGHT F LEFT)))

(defparameter axiom '(LEFT LEFT F))

(defparameter fractal (make-fractal :name "levy"
										:rules (list rule-1)
										:axiom axiom))

(draw fractal 10)


;; Fractal plant

(defaristid F :len 4 :color "black")
(defaristid LEFT :angle 25)
(defaristid RIGHT :angle -25)

(defparameter rule-1 (defrule F -> (F F)))
(defparameter rule-2 (defrule X -> (F RIGHT [ [ X ] LEFT X ] LEFT F [ LEFT F X ] RIGHT X)))

(defparameter axiom '(LEFT LEFT LEFT LEFT LEFT LEFT LEFT LEFT X))

(defparameter fractal (make-fractal :name "fractal-plant"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(draw fractal 4)

;; Stochastic Fractal plant

(defaristid F :len 4 :color "white")
(defaristid G :len 4 :color "salmon")
(defaristid LEFT :angle 25)
(defaristid RIGHT :angle -25)

(defparameter freaky-fractal-rules
	(list (defrule F -> (F G) :prob 0.45)
		  (defrule G -> (F F) )
		  (defrule X -> (F RIGHT [ [ X ] LEFT X ] LEFT F [ LEFT F X ] RIGHT X))))

(defparameter axiom '(LEFT LEFT LEFT LEFT LEFT LEFT LEFT LEFT X))

(defparameter fractal (make-fractal :name "freaky-fractal-plant"
										:rules freaky-fractal-rules
										:axiom axiom))

(draw fractal 7)

(quit)
