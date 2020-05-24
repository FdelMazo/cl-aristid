;;;; cl-dragon.lisp

(in-package #:cl-dragon)

(defun dragon-arr ()
  make-array '(2 3) :initial-contents '((0 1 0) (0 1 0) (0 1 0)))

(defun dragon (n) (
  netpbm:write-to-file (format nil "dragon_~3,'0d.pbm" n) (dragon-arr) :format :pbm :encoding :ascii))
