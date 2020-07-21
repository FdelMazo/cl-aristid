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

(draw fractal 8)


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

;;; Rainbow Quadratic Gosper
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
(defaristid F :len 2 :color "lightblue")
(defaristid RIGHT :angle -90)

(defparameter rule-1 (defrule F -> (F F RIGHT F RIGHT RIGHT F RIGHT F)))

(defparameter axiom '(F RIGHT F RIGHT F RIGHT F))

(defparameter fractal (make-fractal :name "crystal"
										:rules (list rule-1)
										:axiom axiom))

(draw fractal 5)


;;; Von Koch Snowflake
(defaristid F :len 5 :color "white")
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

(defaristid F :len 4 :color "LightGoldenrodYellow")
(defaristid LEFT :angle 25)
(defaristid RIGHT :angle -25)

(defparameter rule-1 (defrule F -> (F F)))
(defparameter rule-2 (defrule X -> (F RIGHT [ [ X ] LEFT X ] LEFT F [ LEFT F X ] RIGHT X)))

(defparameter axiom '(LEFT LEFT LEFT LEFT LEFT LEFT LEFT LEFT X))

(defparameter fractal (make-fractal :name "fractal-plant"
										:rules (list rule-1 rule-2)
										:axiom axiom))

(draw fractal 4)

;; Hilbert Curve
(defaristid F :len 2)
(defaristid LEFT :angle 90)
(defaristid RIGHT :angle -90)

(defparameter hilbert-curve-rules
	(list (defrule A -> (LEFT B F RIGHT A F A RIGHT F B LEFT))
		 (defrule B -> (RIGHT A F LEFT B F B LEFT F A RIGHT))))

(defparameter hilbert-curve-axiom '(A))

(defparameter fractal (make-fractal :name "hilbert-curve"
										:rules hilbert-curve-rules
										:axiom hilbert-curve-axiom))

(draw fractal 6)

;; Davis-Knuth Dragon
(defaristid F :len 2 :color "salmon")
(defaristid A :len 0 :nodraw t :color "salmon")
(defaristid B :len 0 :nodraw t :color "salmon")
(defaristid LEFT :angle 90)
(defaristid RIGHT :angle -90)
(defparameter fractal (make-fractal :name "davis-knuth-dragon"
									:rules (list
										(defrule A -> (A RIGHT B F))
										(defrule B -> (F A LEFT B)))
									:axiom '(F A RIGHT F A RIGHT)))
(draw fractal 10)


;; Saupe Bush
(defaristid F :len 5 :color "lightgreen")
(defaristid LEFT :angle 20)
(defaristid RIGHT :angle -20)
(defparameter fractal (make-fractal :name "saupe-bush"
									:rules (list
										(defrule V -> ([ RIGHT RIGHT RIGHT RIGHT W ] [ LEFT LEFT LEFT W ] Y V))
										(defrule W -> (RIGHT X [ LEFT W ] Z))
										(defrule X -> (LEFT W [ RIGHT X ] Z))
										(defrule Y -> (Y Z))
										(defrule Z -> ([ LEFT F F F ] [ RIGHT F F F ] F)))
									:axiom '(V Z F F F)))
(draw fractal 10)


(defaristid A :len 2 :color "PaleTurquoise")
(defaristid B :len 2 :color "Plum")
(defaristid LEFT :angle 60)
(defaristid RIGHT :angle -60)
(defparameter fractal (make-fractal :name "peano-gosper-curve"
									:rules (list
										(defrule A -> (A LEFT B LEFT LEFT B RIGHT A RIGHT RIGHT A A RIGHT B LEFT))
										(defrule B -> (RIGHT A LEFT B B LEFT LEFT B LEFT A RIGHT RIGHT A RIGHT B)))
									:axiom '(A)))
(draw fractal 4)

(defaristid F :len 5 :color "AntiqueWhite")
(defaristid LEFT :angle 45)
(defaristid RIGHT :angle -45)
(defparameter fractal (make-fractal :name "weirdstuff"
									:rules (list
										(defrule F -> (F LEFT LEFT LEFT F RIGHT F RIGHT F RIGHT F RIGHT F RIGHT F RIGHT F LEFT LEFT LEFT F)))
									:axiom '(F LEFT F LEFT F LEFT F LEFT F LEFT F LEFT F LEFT F)))
(draw fractal 3)

(defaristid F :len 3 :color "aqua")
(defaristid LEFT :angle 45)
(defaristid RIGHT :angle -45)
(defparameter fractal (make-fractal :name "epholys-snowflake"
									:rules (list
										(defrule X -> (F [ LEFT X ] [ RIGHT X ] F F X))
										(defrule F -> (F F)))
									:axiom '([ X ] [ LEFT X ] [ RIGHT X ] [ LEFT LEFT X ] [ RIGHT RIGHT X ] [ LEFT LEFT LEFT X ] [ RIGHT RIGHT RIGHT X ] [ RIGHT RIGHT RIGHT RIGHT X ])))
(draw fractal 5)

(defaristid F :len 2)
(defaristid B :len 2 :nodraw t)
(defaristid LEFT :angle 90)
(defaristid RIGHT :angle -90)
(defparameter fractal (make-fractal :name "island"
									:rules (list
										(defrule B -> (b b b b b b))
										(defrule F -> (F LEFT B RIGHT F F LEFT F LEFT F F LEFT F B LEFT F F RIGHT B LEFT F F RIGHT F RIGHT F F RIGHT F B RIGHT F F F)))
									:axiom '(F LEFT F LEFT F LEFT F)))
(draw fractal 2)


(defaristid F :len 2 :color "Wheat")
(defaristid G :len 2 :nodraw t)
(defaristid X :len 2)
(defaristid LEFT :angle 9)
(defaristid RIGHT :angle -9)
(defparameter fractal (make-fractal :name "vibes"
									:rules (list
										(defrule F -> (F [ RIGHT RIGHT G RIGHT F ] LEFT LEFT LEFT [ LEFT LEFT F ]))
										(defrule G -> (G G))
										(defrule X -> ([ F RIGHT RIGHT F RIGHT RIGHT RIGHT RIGHT F RIGHT RIGHT F RIGHT RIGHT RIGHT F ])))
									:axiom '(X)))
(draw fractal 7)


(defaristid X :len 0)
(defaristid F :len 5 :color "MediumSlateBlue")
(defaristid LEFT :angle 15)
(defaristid RIGHT :angle -15)
(defparameter fractal (make-fractal :name "mandala"
									:rules (list
										(defrule F -> (F F [ RIGHT RIGHT F ] [ LEFT LEFT F ]))
										(defrule X -> ([ F ] [ LEFT F ] [ RIGHT F ] [ LEFT LEFT F ] [ RIGHT RIGHT F ] [ LEFT LEFT LEFT F ] [ RIGHT RIGHT RIGHT F ] [ LEFT LEFT LEFT LEFT F ] [ RIGHT RIGHT RIGHT RIGHT F ] )))
									:axiom '(X RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT X RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT RIGHT X)))
(draw fractal 4)

;; Somewhat occult-looking fractal
(defaristid F :len 5 :color "rainbow")
(defaristid LEFT :angle 72)

(defparameter occult-rules
    (list (defrule F -> (F F LEFT F LEFT F LEFT F LEFT F LEFT F F))))

(defparameter occult-axiom '(F LEFT F LEFT F LEFT F LEFT F LEFT F))

(defparameter fractal (make-fractal :name "somewhat-occult"
                                        				:rules occult-rules
                                        				:axiom occult-axiom))

(draw fractal 3)

;; The Phoenix (inspired by fractal tree structures)
(defaristid A :len 5 :color "gold")
(defaristid B :len 5 :color "red")
(defaristid P :len 5 :color "orangered")
(defaristid LEFT :angle 30)
(defaristid RIGHT :angle -30)

(defparameter phoenix-rules
	(list (defrule B -> (A [ RIGHT B ] [ B ] [ LEFT B ] P))))

(defparameter phoenix-axiom '(B A P))

(defparameter fractal (make-fractal :name "the-phoenix"
										:rules phoenix-rules
										:axiom phoenix-axiom))
(draw fractal 8)

(quit)
