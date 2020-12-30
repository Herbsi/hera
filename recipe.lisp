;;;; recipe.lisp

(in-package #:hera)

(defclass/std recipe ()
  ((name :ri)
   (ingredients :ri)))


(defun clean-ingredient-line (line)
  "Takes a line containing one ingredient, “cleans it up”
and returns the cleaned up line.  For example, it
removes leading ‘*’:

(clean-ingredient-line \"* 1 large onion\") => \"1 large onion\" "
  
  (subseq line 2))


(defun extract-ingredients (recipe-string)
  "Returns the ingredients of recipe as a list of strings"
  (multiple-value-bind (_ ingredients)
      (ppcre:scan-to-strings "##? Ingredients\\n((?:.*?\\n)+?)#" recipe-string)
    (declare (ignore _))
    (gmap :list #'clean-ingredient-line
          (:list (str:split #\Newline
                            (aref ingredients 0)
                            :omit-nulls t)))))


(defun make-recipe (recipe-path)
  "Creates a recipe object from the recipe at `recipe-path'"
  (when-let ((recipe-string (restart-case (uiop:read-file-string recipe-path)
                              (retry-filename (filename)
                                (uiop:read-file-string
                                 (substitute-filename recipe-path filename))))))
    (make-instance 'recipe :name (extract-title recipe-string)
                           :ingredients (extract-ingredients recipe-string))))


(defun substitute-filename (pathname filename)
  "Returns a new path that corresponds to `pathname', but ending in `filename' instead,

i.e. (replace-filename (#P\"/foo/bar\" \"baz\") => #P\"/foo/baz\""

  (cl-fad:merge-pathnames-as-file (uiop:pathname-directory-pathname pathname)
                                  filename))
