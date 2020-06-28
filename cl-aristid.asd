(asdf:defsystem #:cl-aristid
  :description "Export Lindenmayer Systems to SVG with Common LISP!"
  :author "https://github.com/FdelMazo/cl-aristid"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-svg #:cl-colors)
  :components ((:file "package")
               (:file "cl-aristid")
               (:file "canvas")
               (:file "fractal")))
