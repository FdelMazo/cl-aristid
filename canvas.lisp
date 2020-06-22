(in-package #:cl-aristid)

(defstruct (canvas (:constructor %make-canvas))
  matrix
  dir
  point
  prev-point
  corners ;left top right bottom
  stack)

(setq *random-state* (make-random-state t))
(setq *random-red* (random 255))
(setq *random-green* (random 255))
(setq *random-blue* (random 255))

(defun random-increase (color)
  (if (<= (random 1.00) 0.80)
      (if (<= (random 1.00) 0.50)
          (if (< color 255) (setq color (+ color 1)))
          (if (> color 0) (setq color (- color 1)))))
  color)

(defun get-next-color ()
  (setq *random-red* (random-increase *random-red*))
  (setq *random-green* (random-increase *random-green*))
  (setq *random-blue* (random-increase *random-blue*))
  (format nil "#~2,'0X~2,'0X~2,'0X" *random-red* *random-green* *random-blue*))

(defun make-canvas ()
  (let ((matrix (cl-svg:make-group
                  (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)
                  (:stroke "black")))
        (init-point (list 5000 5000)))
    (%make-canvas :matrix matrix :dir (list 0 1)
                              :point init-point :prev-point init-point
                              :corners (list 5000 5000 5000 5000)
                              :stack (list))))

(defun draw-point (canvas &optional (color ""))
  (if (string= color "rainbow") (setq color (get-next-color)))
  (cl-svg:draw (canvas-matrix canvas)
               (:line  :x1 (first (canvas-prev-point canvas))
                       :y1 (second (canvas-prev-point canvas))
                       :x2 (first (canvas-point canvas))
                       :y2 (second (canvas-point canvas)))
                       :stroke color)
    canvas)

(defun canvas-move (canvas)
  (setf (canvas-prev-point canvas) (copy-list (canvas-point canvas)))
  (map-into (canvas-point canvas) #'+ (canvas-point canvas) (canvas-dir canvas))
  (destructuring-bind (x y left top right bottom)
                      (append (canvas-point canvas)
                              (canvas-corners canvas))
    (if (< x left) (setq left x))
    (if (< y top) (setq top y))
    (if (> x right) (setq right x))
    (if (> y bottom) (setq bottom y))
    (setf (canvas-corners canvas) (list left top right bottom)))
  canvas)

(defun d2r (degrees) (* pi (/ degrees 180.0)))

(defun rotate-dir (dir angle)
  (destructuring-bind (x y) dir
    (list (- (* x (cos (d2r angle)))
             (* y (sin (d2r angle))))
          (+ (* x (sin (d2r angle)))
             (* y (cos (d2r angle)))))))

(defun turn-angle (canvas angle)
  (setf (canvas-dir canvas) (rotate-dir (canvas-dir canvas) angle))
  canvas)

(defun create-svg (canvas)
  (destructuring-bind (left top right bottom) (canvas-corners canvas)
    (let ((w (- right left)) (h (- bottom top)))
      (if (zerop w) (setq w 1))
      (if (zerop h) (setq h 1))
      (cl-svg:make-svg-toplevel
          'cl-svg:svg-1.1-toplevel
          :width (* 100 w) :height (* 100 h)
          :view-box (format nil "~,2f ~,2f ~,2f ~,2f" left top w h)))))

(defun push-stack (canvas)
  (setf (canvas-stack canvas)
    (cons
       (list (copy-list (canvas-point canvas))
             (copy-list (canvas-prev-point canvas))
             (copy-list (canvas-dir canvas)))
        (canvas-stack canvas)))
  canvas)

(defun pop-stack (canvas)
  (destructuring-bind (point prev-point dir) (pop (canvas-stack canvas))
    (setf (canvas-point canvas) point)
    (setf (canvas-prev-point canvas) prev-point)
    (setf (canvas-dir canvas) dir))
  canvas)

(defun svg-add-iteration (svg canvas)
  (cl-svg:add-element svg (canvas-matrix canvas))
  svg)
