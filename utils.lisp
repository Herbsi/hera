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


(defun append-list-to-file (file list)
  "Appends the list to file, separated by #\Newline"
  (with-open-file (*standard-output* file :direction :output :if-exists :append :if-does-not-exist :create)
    (format t "~&~{~A~^~%~}~%" list)))


(defun read-list-from-file (file &optional (sep #\Newline))
  "Convenience function for reading a list from file

List should be separated by `sep' in the file"
  (str:split sep (uiop:read-file-string file) :omit-nulls t))


(defun execute-osascript (script)
  "Executes `script' which should be an AppleScript string."
  (inferior-shell:run
   (format nil "osascript ~{-e \"~a\"~^ ~}" (str:split #\Newline script))))


(defun set-body-of-apple-note (content note-id)
  "Sets the body of the Apple Note with id `note-id' to `content'"
  ;; We put three backslashes before each quote
  ;; 1 to escape the quote; 2 more to put an additional backslash there because the script gets
  ;; passed to the shell, which swallows another backslash to (still) escape the quotes
  (let ((script "tell application \\\"Notes\\\"
	set new_body to \\~s\\\
	set note_id to \\~s\\
	set note_name to get name of note id note_id
	set body of note id note_id to \\\"<div><h1>\\\" & note_name & \\\"</h1></div>\\\" & new_body
end tell"))
    (execute-osascript (format nil script content note-id))))

(defun add-task-to-omnifocus-project (task-name project-name due-date defer-date)
  "Adds the task named `task-name' to the project named `project-name'
with {due,defer}-date set to `{due,defer}-date'"
  (let ((script "set task_name to \\~s\\
set project_name to \\~s\\
set due_date to date \\~s\\
set defer_date to date \\~s\\
tell application \\\"OmniFocus\\\"
	tell default document
		make new task at (project project_name) ¬
			with properties {name:task_name, due date:due_date, defer date:defer_date}
	end tell
end tell"))
    (execute-osascript
     (format nil script task-name project-name due-date defer-date))))


(defun add-item-to-reminders (item-name reminders-list)
  "Adds the item named `item-name' to the reminders list `reminders-list'"
  (execute-osascript "Add Item to Reminders.scpt" item-name reminders-list))
