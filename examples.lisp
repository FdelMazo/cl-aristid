(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload "cl-aristid")

;;; We set up the fractal
;; We set up the aristids (i.e the drawing functions)
(cl-aristid:defaristid F :len 2)
(cl-aristid:defaristid LEFT :angle 90)
(cl-aristid:defaristid RIGHT :angle -90)

;; We set up the production rules
(defparameter dragon-rule-1 (cl-aristid:-> 'A '(A RIGHT B F RIGHT)))
(defparameter dragon-rule-2 (cl-aristid:-> 'B '(LEFT F A LEFT B)))

;; We set up the fractal axiom
(defparameter dragon-axiom '(F A))

;; We create the fractal
(defparameter fractal (cl-aristid:make-fractal :name "dragon"
										:rules (list dragon-rule-1 dragon-rule-2)
										:axiom dragon-axiom))

;;; We draw the fractal
(cl-aristid:draw fractal 3)

;;; We do it again with a new fractal

;; We add or modify the aristids
(cl-aristid:defaristid A :len 5)
(cl-aristid:defaristid B :len 5)
(cl-aristid:defaristid LEFT :angle 120)
(cl-aristid:defaristid RIGHT :angle -120)

;; We set up the production rules
(defparameter triangle-rule-1 (cl-aristid:-> 'A '(A LEFT B RIGHT A RIGHT B LEFT A)))
(defparameter triangle-rule-2 (cl-aristid:-> 'B '(B B)))

;; We set up the new axiom
(defparameter triangle-axiom '(A LEFT B LEFT B))

;; We create the fractal
(defparameter triangle-fractal (cl-aristid:make-fractal :name "triangle"
										:rules (list triangle-rule-1 triangle-rule-2)
										:axiom triangle-axiom))

;;; We draw the fractal
(cl-aristid:draw triangle-fractal 5)
(quit)
