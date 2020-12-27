;;;; mealplan.lisp

(in-package #:hera)

(defconstant *weekdays* '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

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


(defun collect-ingredients (mealplan)
  "Collects all ingredients into a list"
  ;; TODO
  nil
  )


(defun format-meal-for-apple-note (day meal)
  (format nil "~a: ~a" (str:title-case (string day))
          (case (kind meal)
            (:lunch "Cooks Lunch")
            (:dinner "Cooks Dinner")
            (:bake "Bakes something"))))


(defun add-mp-to-apple-notes (mealplan note-id)
  "Adds the mealplan to the Apple Note specified by `note-id'

The days are in order, i.e. Monday comes before Tuesday, etc."
  (with-accessors ((meals meals)) mealplan
    (let ((content (iter
                     (for day in *weekdays*)
                     (collect (mapcar (fn (meal) (when (apple-notes meal) (format-meal day meal)))
                                      (gethash day meals))
                       into result)
                     (finally (return (format nil "~&~a~a"
                                              (xml-header "Herwig Cooks")
                                              (xml-unordered-list (remove nil (alexandria:flatten result)))))))))
      (set-body-of-apple-note content note-id))))


(defun add-mp-to-omnifocus (mealplan)
  ;; TODO
  nil
  )
