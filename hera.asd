;;;; hera.asd

(asdf:defsystem #:hera
  :description "Describe hera here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:iterate #:str #:cl-utilities)
  :components ((:file "package")
               (:file "hera")))
