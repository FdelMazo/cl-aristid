(asdf:defsystem #:cl-aristid
  :description " Draw Lindenmayer Systems with Common LISP!"
  :author "FdelMazo, JDSanto"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-svg #:cl-colors)
  :components ((:file "package")
               (:file "cl-aristid")
               (:file "canvas")
               (:file "fractal")))
