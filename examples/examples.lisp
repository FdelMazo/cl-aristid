(pushnew (truename "..") asdf:*central-registry*)
(ql:quickload "cl-aristid")
(use-package 'cl-aristid)

;;; We set up the fractal
;; We set up the aristids (i.e the drawing functions)
(defaristid F :len 2)
(defaristid LEFT :angle 90)
(defaristid RIGHT :angle -90)

;; We set up the production rules
(defparameter dragon-rules
	(list (defrule A -> (A RIGHT B F RIGHT))
		  (defrule B -> (LEFT F A LEFT B))))

;; We set up the fractal axiom
(defparameter dragon-axiom '(F A))

;; We create the fractal
(defparameter fractal (make-fractal :name "dragon"
										:rules dragon-rules
										:axiom dragon-axiom))

;;; We draw the fractal
(draw fractal 10)

;;; We do it again with a new fractal

;; We add or modify the aristids
(defaristid A :len 5 :color "crimson")
(defaristid B :len 5 :color "crimson")
(defaristid LEFT :angle 120)
(defaristid RIGHT :angle -120)

;; We set up the production rules
(defparameter triangle-rules
	(list (defrule A -> (A LEFT B RIGHT A RIGHT B LEFT A))
		  (defrule B -> (B B))))

;; We set up the new axiom
(defparameter triangle-axiom '(A LEFT B LEFT B))

;; We create the fractal
(defparameter triangle-fractal (make-fractal :name "triangle"
										:rules triangle-rules
										:axiom triangle-axiom))

;;; We draw the fractal
(draw triangle-fractal 5)


;;; Now we'll try using brackets!

;; We add or modify the aristids
(defaristid A :len 8 :color "purple")
(defaristid B :len 8 :color "white")
(defaristid LEFT :angle 45)
(defaristid RIGHT :angle -45)

;; We set up the production rules, which include the push and pop aristids
(defparameter tree-rule-1 (defrule A -> (B [ RIGHT A ] LEFT A)))
(defparameter tree-rule-2 (defrule B -> (B B)))

;; We set up the new axiom
(defparameter tree-axiom '(LEFT LEFT LEFT LEFT A))

;; We create the fractal
(defparameter tree-fractal (make-fractal :name "binary-tree"
										:rules (list tree-rule-1 tree-rule-2)
										:axiom tree-axiom))

;;; We draw the fractal
(draw tree-fractal 6)
(quit)
