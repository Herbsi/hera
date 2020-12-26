;;;; hera.lisp

(in-package #:hera)
(defclass/std recipe ()
  ((name :ri)
   (ingredients :ri)))


(defun extract-title (recipe-string)
  "Extracts the title of a recipe string and returns it as a string"
  (multiple-value-bind (_ title)
      (ppcre:scan-to-strings "# (.*?)\\n" recipe-string)
    (declare (ignore _))
    (aref title 0)))
