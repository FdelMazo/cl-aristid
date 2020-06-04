(in-package #:cl-aristid)

(defstruct (canvas (:constructor %make-canvas))
  matrix
  dir
  point
  prev-point
  corners) ;left top right bottom

(defun make-canvas ()
  (let ((matrix (cl-svg:make-group
                  (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)
                  (:id "fractal")))
        (init-point (list 5000 5000)))
    (draw-point (%make-canvas :matrix matrix :dir (list 0 1)
                              :point init-point :prev-point init-point
                              :corners (list 5000 5000 5000 5000)))))

(defun draw-point (canvas &optional (color "black"))
  (cl-svg:draw (canvas-matrix canvas)
               (:line  :x1 (first (canvas-prev-point canvas))
                       :y1 (second (canvas-prev-point canvas))
                       :x2 (first (canvas-point canvas))
                       :y2 (second (canvas-point canvas))
                       :stroke color))
    canvas)

(defun canvas-move (canvas)
  (setf (canvas-prev-point canvas) (copy-list (canvas-point canvas)))
  (map-into (canvas-point canvas) #'+ (canvas-point canvas) (canvas-dir canvas))
  (if (< (first (canvas-point canvas)) (first (canvas-corners canvas)))
      (setf (first (canvas-corners canvas)) (first (canvas-point canvas))))
  (if (< (second (canvas-point canvas)) (second (canvas-corners canvas)))
      (setf (second (canvas-corners canvas)) (second (canvas-point canvas))))
  (if (> (first (canvas-point canvas)) (third (canvas-corners canvas)))
      (setf (third (canvas-corners canvas)) (first (canvas-point canvas))))
  (if (> (second (canvas-point canvas)) (fourth (canvas-corners canvas)))
      (setf (fourth (canvas-corners canvas)) (second (canvas-point canvas))))
  canvas)

(defun d2r (degrees) (* pi (/ degrees 180.0)))

(defun rotate-dir (dir angle)
  (list (- (* (first dir) (cos (d2r angle))) (* (second dir) (sin (d2r angle))))
        (+ (* (first dir) (sin (d2r angle))) (* (second dir) (cos (d2r angle))))))

(defun turn-angle (canvas angle)
  (setf (canvas-dir canvas) (rotate-dir (canvas-dir canvas) angle))
  canvas)

(defun create-svg (canvas)
  (destructuring-bind (x y w h)
                      (mapcar #'round
                        (list (first (canvas-corners canvas))
                                (second (canvas-corners canvas))
                                (- (third (canvas-corners canvas))
                                   (first (canvas-corners canvas)))
                                (- (fourth (canvas-corners canvas))
                                   (second (canvas-corners canvas)))))
    (setq svg (cl-svg:make-svg-toplevel
                  'cl-svg:svg-1.1-toplevel
                  :width (* 100 w) :height (* 100 h)
                  :view-box (format nil "~d ~d ~d ~d" x y w h)))
    (cl-svg:add-element svg (canvas-matrix canvas))
    svg))
