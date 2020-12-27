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


(defun append-list-to-file (file lis)
  "Appends the list to file, separated by #\Newline"
  (with-open-file (*standard-output* file :direction :output :if-exists :append)
    (format t "~&{~A~^~%}~%" list)))


(defun read-list-from-file (file &optional (sep #\Newline))
  "Convenience function for reading a list from file

List should be separated by `sep' in the file"
  (str:split sep (uiop:read-file-string file) :omit-nulls t))


(defun set-body-of-apple-note (content note-id)
  "Sets the body of the Apple Note with id `note-id' to `content'"
  (inferior-shell:run (format nil "osascript \"Set Body of Note.scpt\" \"~a\" \"~a\"" note-id content)))
