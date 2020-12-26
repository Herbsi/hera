;;;; mealplan.lisp

(in-package #:hera)

(defparameter *mealplan* (uiop:read-file-string "~/Notes/Meal Plan.md"))

(defclass/std mealplan ()
  ((meals :ri)))


(defclass/std meal ()
  ((name :ri)
   (kind :ri :std :lunch)))


(defun extract-meals-on-day (mealplan-string day)
  "For a given `day', extracts the meals being cooked on that day
and returns a vector of corresponding meal objects."
  (multiple-value-bind (_ meals)
      (ppcre:scan-to-strings (str:concat "# " day "\\s*\\n((?:[-*] .*?\\n)+)")
                             mealplan-string)
    (declare (ignore _))
    (unless (null meals)
      (gmap :vector #'make-meal (:list (str:split #\Newline (aref meals 0)
                                                  :omit-nulls t))))))

(defun make-meal (meal-line)
  "Takes a meal line and returns a meal object based on it."
  (make-instance 'meal :name (subseq meal-line 2)))


(defun make-mealplan (mealplan-string)
  (let ((meals (make-hash-table :test #'eq)))
    (dolist (day '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
      (setf (gethash (make-keyword day) meals)
            (extract-meals-on-day mealplan-string day)))
    (make-instance 'mealplan :meals meals)))
