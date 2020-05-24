;;;; cl-dragon.asd

(asdf:defsystem #:cl-dragon
  :description "Describe cl-dragon here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-netpbm)
  :components ((:file "package")
               (:file "cl-dragon")))
