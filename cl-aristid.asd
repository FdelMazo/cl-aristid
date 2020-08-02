(asdf:defsystem #:cl-aristid
  :description " Draw Lindenmayer Systems with Common LISP!"
  :author "FdelMazo, JDSanto, camidvorkin, anitasec"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:cl-svg #:cl-colors)
  :components ((:file "package")
               (:file "cl-aristid")
               (:file "canvas")
               (:file "fractal")))
