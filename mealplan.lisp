;;;; mealplan.lisp

(in-package #:hera)

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
                  (mapcar (fn (day) (cons day (fset:empty-set)))
                          '(:monday :tuesday :wednesday
                            :thursday :friday :saturday :sunday)))))


(defun add-meal (meal mealplan day)
  "Adds `meal' to `mealplan' on `day' if it’s not already present.

Assums `day' is a keyword for a weekday."
  (with-accessors (meals meals) mealplan
    (setf (gethash day meals)
          (fset:adjoinf (gethash day meals) meal))))


(defun write-ingredients-to-tmp (mealplan &optional (target #P"/tmp/ingredients"))
  "Writes all ingredients on the mealplan to `target', appending to the file
if it already existis"
  (with-open-file (*standard-output* target :direction :output :if-exists :append)
    (iter (for (nil meals) in-hashtable (meals mealplan))
      (fset:do-set (meal meals)
        (format t "~&~{~A~^~%~}~%" (ingredients (recipe meal)))))))


(defun add-mp-to-apple-notes (mealplan note-id)
  (with-accessors ((meals meals))))
