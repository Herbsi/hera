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


(defun clean-ingredient-line (line)
  "Takes a line containing one ingredient, “cleans it up”
and returns the cleaned up line.  For example, it
removes leading ‘*’:

(clean-ingredient-line \"* 1 large onion\") => \"1 large onion\" "
  
  (subseq line 2))


(defun extract-ingredients (recipe-string)
  "Returns the ingredients of recipe as a vector of strings"
  (multiple-value-bind (_ ingredients)
      (ppcre:scan-to-strings "##? Ingredients\\n((?:.*?\\n)+?)#" recipe-string)
    (declare (ignore _))
    (gmap :vector #'clean-ingredient-line
          (:list (str:split #\Newline
                            (aref ingredients 0)
                            :omit-nulls t)))))


(defun make-recipe (recipe-string)
  (make-instance 'recipe :name (extract-title recipe-string)
                         :ingredients (extract-ingredients recipe-string)))

