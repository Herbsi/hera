;;;; hera.asd

(asdf:defsystem #:hera
  :description "My Grocery Automation"
  :author "Herwig Hoehenberger <herwig.hoehenberger@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-ppcre
               #:defclass-std
               #:fset
               #:iterate #:misc-extensions #:str)
  :components ((:file "package")
               (:file "hera" :depends-on ("package"))
               (:file "recipe" :depends-on ("package"))
               (:file "utils" :depends-on ("package"))))
