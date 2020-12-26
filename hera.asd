;;;; hera.asd

(asdf:defsystem #:hera
  :description "My Grocery Automation"
  :author "Herwig Hoehenberger <herwig.hoehenberger@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:iterate #:misc-extensions #:str)
  :components ((:file "package")
               (:file "hera")))
