;;;; mealplan.lisp

(in-package #:hera)

(defparameter *weekdays* '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

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


(defun make-meal (recipe-pathname &key (kind :lunch) (apple-notes t) (devonthink nil))
  "Creates an instance of `meal'.  Allows for the caller to specify a special return value
via (invoke-restart 'return-value value)"
  (assert (member kind '(:lunch :dinner :bake)))
  (restart-case (make-instance 'meal
                               :recipe (make-recipe recipe-pathname)
                               :kind kind
                               :apple-notes apple-notes
                               :devonthink devonthink)
    (return-value (value) value)))


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
  "Collects all ingredients into a list and returns it" 
  (mappend (compose #'ingredients #'recipe)
           (iter
             (for (nil meals) in-hashtable (meals mealplan))
             ;; take each recipe only once, even if it lasts for multiple days
             ;; I never cook the same recipe twice in a week
             (unioning meals :test (fn (meal-1 meal-2)
                                     (string= (name (recipe meal-1))
                                              (name (recipe meal-2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  Adding the Mealplan to Apple Notes ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun meal-format-for-apple-note (day-of-week meal)
  (format nil "~a: ~a" (str:title-case (string day-of-week))
          (case (kind meal)
            (:lunch "Cooks Lunch")
            (:dinner "Cooks Dinner")
            (:bake "Bakes something"))))


(defun mealplan-add-to-apple-notes (mealplan note-id)
  "Adds the mealplan to the Apple Note specified by `note-id'

The days are in order, i.e. Monday comes before Tuesday, etc."
  (let ((content (iter
                   (for (day-of-week meals) in-hashtable (meals mealplan))
                   (collect (mapcar (fn (meal) (when (apple-notes meal) (meal-format-for-apple-note day-of-week meal)))
                                    meals)
                     into result)
                   (finally (return (format nil "~&~a~%"
                                            (xml-unordered-list (remove nil (alexandria:flatten result)))))))))
    (set-body-of-apple-note content note-id)))


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


(defun meal-due-time (meal)
  "Determines the due-time for cooking `meal'"
  (case (kind meal)
    (:lunch "12:30")
    (:dinner "18:00")
    (:bake "16:30")))

(defun mealplan-add-to-omnifocus (mealplan project-name)
  "Adds the `mealplan' the Omnifocus project named `project-name'"
  (flet ((meal-due-date (meal day-of-week)
           (format nil "~a ~a" (next-iso-day day-of-week) (meal-due-time meal)))
         (defer-date (day-of-week)
           (format nil "~a 11:00" (next-iso-day day-of-week))))
    (iter (for (day-of-week meals) in-hashtable (meals mealplan))
      (iter
        (for meal in meals)
        (for task-name = (format nil "~a ~a"
                                 (if (eq (kind meal) :bake) "Bake" "Cook")
                                 (name (recipe meal))))
        (for due-date = (meal-due-date meal day-of-week))
        (for defer-date = (defer-date day-of-week))
        (add-task-to-omnifocus-project task-name project-name due-date defer-date)))))
