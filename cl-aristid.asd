(asdf:defsystem #:cl-aristid
  :description "Draw Lindenmayer Systems with Common LISP!"
  :author "https://github.com/FdelMazo/cl-aristid"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-netpbm #:magicl #:cl-svg)
  :components ((:file "package")
               (:file "cl-aristid")
               (:file "canvas")
               (:file "fractal")))
