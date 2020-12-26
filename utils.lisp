;;;; utils.lisp

(in-package #:hera)

(defun extract-title (markdown-string)
  "Extracts the title of a recipe string and returns it as a string"
  (multiple-value-bind (_ title)
      (ppcre:scan-to-strings "# (.*?)\\n" markdown-string)
    (declare (ignore _))
    (aref title 0)))


(defun make-keyword (string)
  "Returns a keyword from string.  Upcase string first so that

(eq :foo (make-keyword \"foo\")) => T"
  (alexandria:make-keyword (str:upcase string)))


