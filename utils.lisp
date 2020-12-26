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


(defun xml-header (string &optional (level 1))
  "Wraps `string' into an xml-header format."
  (format nil "~&<div><h~A>~A</h~a></div>~%" level string level))


(defun xml-unordered-list (list-of-strings)
  "Turns `list-of-strings' into an unordered xml-list"
  (format nil "~&<ul>~&~{<li>~A</li>~^~%~}~%</ul>~%" list-of-strings))
