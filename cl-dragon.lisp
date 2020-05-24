;;;; cl-dragon.lisp

(in-package #:cl-dragon)

(defun dragon-arr (n m)
  (let ((array (make-array (list n m) :initial-element 1)))
    (setf (aref array 1 2) 0) array))

(defun dragon (n) (
  netpbm:write-to-file (format nil "dragon_~3,'0d.pbm" n) (dragon-arr 50 50) :format :pbm :encoding :ascii :if-exists :overwrite))

