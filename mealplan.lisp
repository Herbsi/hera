;;;; mealplan.lisp

(in-package #:hera)

(defconstant *weekdays* '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Meal & Mealplan           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass/std mealplan ()
  ((meals :std (make-hash-table :test #'eq))))


(defclass/std meal ()
  ((recipe :ri)
   (kind :ri :std :lunch)
   (apple-notes :ri :std t)
   (devonthink :ri)))


(defun make-meal (recipe-filename &key (kind :lunch) (apple-notes t) (devonthink nil))
  "Takes a meal line and returns a meal object based on it."
  (assert (member kind '(:lunch :dinner :bake)))
  (make-instance 'meal
                 :recipe (make-recipe
                          (cl-fad:merge-pathnames-as-file *recipe-root* recipe-filename))
                 :kind kind
                 :apple-notes apple-notes
                 :devonthink devonthink))


(defun make-mealplan ()
  "Returns a new mealplan object"
  (make-instance 'mealplan
                 :meals
                 (alexandria:alist-hash-table
                  (mapcar (fn (day) (list day)) *weekdays*))))


(defun add-meal (meal mealplan day)
  "Adds `meal' to `mealplan' on `day' if it’s not already present.

Assums `day' is a keyword for a weekday."
  (with-accessors ((meals meals)) mealplan
    (setf (gethash day meals)
          (cons meal (gethash day meals)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  Adding Ingredients to Reminders/OF ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun collect-ingredients (mealplan)
  "Collects all ingredients into a list"
  ;; TODO
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  Adding the Mealplan to Apple Notes ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun meal-format-for-apple-note (day meal)
  (format nil "~a: ~a" (str:title-case (string day))
          (case (kind meal)
            (:lunch "Cooks Lunch")
            (:dinner "Cooks Dinner")
            (:bake "Bakes something"))))


(defun mealplan-add-to-apple-notes (mealplan note-id)
  "Adds the mealplan to the Apple Note specified by `note-id'

The days are in order, i.e. Monday comes before Tuesday, etc."
  (with-accessors ((meals meals)) mealplan
    (let ((content (iter
                     (for day in *weekdays*)
                     (collect (mapcar (fn (meal) (when (apple-notes meal) (meal-format-for-apple-note day meal)))
                                      (gethash day meals))
                       into result)
                     (finally (return (format nil "~&~a~a"
                                              (xml-header "Herwig Cooks")
                                              (xml-unordered-list (remove nil (alexandria:flatten result)))))))))
      (set-body-of-apple-note content note-id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        Adding Mealplan to OF        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun next-ISO-day (day-of-week)
  "Returns the next date that is a `day-of-week' as an yyyy-mm-dd strin

If `day-of-week' is the same as today, *next week’s* `day-of-week' is returned,
i.e. if it’s Sun, Dec 27 2020; then (next-ISO-day :sunday) => \"2021-01-03\""
  (let* ((timestamp-day-of-week-lookup (alexandria:alist-hash-table '((0 . :sunday)
                                                                      (1 . :monday)
                                                                      (2 . :tuesday)
                                                                      (3 . :wednesday)
                                                                      (4 . :thursday)
                                                                      (5 . :friday)
                                                                      (6 . :saturday))))
         (today (local-time:today))
         (today-day-of-week (gethash (local-time:timestamp-day-of-week today) timestamp-day-of-week-lookup))
         ;; We want the *next* `day-of-week'
         ;; offset :day-of-week `day-of-week' gives us the previous, between 1 & 7 days back
         ;; i.e. if it’s sunday, thet (adjust-timestamp (today) (offset :day-of-week :sunday)) => previous sunday
         ;; so in that case, we add 14 days, otherwise we just add 7
         (adjusted-date (local-time:adjust-timestamp today (offset :day-of-week day-of-week)
                          (offset :day (if (eq today-day-of-week day-of-week) 14 7)))))
    (local-time:format-timestring nil adjusted-date :format '(:year #\-
                                                              (:month 2 #\0) #\-
                                                              (:day 2 #\0)))))


(defun meal-due-time ()
  ;; TODO
  nil
  )

(defun mealplan-add-to-omnifocus (mealplan)
  ;; TODO
  nil
  )
