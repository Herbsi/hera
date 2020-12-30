;;;; hera.lisp

(in-package #:hera)

(defparameter *recipe-root* #P"~/Notes/Recipes/")
(defparameter *apple-notes-id*  "x-coredata://F93C847E-0906-4FFC-881E-472B69BB9B9C/ICNote/p261")
(defparameter *omnifocus-project* "🏡 Housework")
(defparameter *reminders-list* "Grocery")
(defparameter *tmp-file* "/tmp/ingredients")

(defun transform-meal (meal-list mealplan day)
  "Transform one (<meal> args) list into a

(add-meal (make-meal <meal> args) mealplan day) list for adding
the meal to the mealplan."
  (with-gensyms (meal)
    (flet ((prepend-recipe-root (filename)
             (cl-fad:merge-pathnames-as-file *recipe-root* filename)))
      `(let ((,meal (make-meal ,@(cons (prepend-recipe-root (first meal-list))
                                       (rest meal-list)))))
         (unless (eq ,meal :skipped)
           (add-meal ,meal ,mealplan ,day))))))


(defun transform-day (day-list mealplan)
  "Processes a day-list like (<day> (<meal> args)+)

into a list of (add-meal (make-meal <meal> args) <day>) function calls."
  (let ((day (make-keyword (first day-list))))
    (mapcar (fn (meal-list) (transform-meal meal-list mealplan day))
            (rest day-list))))


(defun deal-with-file-error (condition)
  "Asks user for a new filename fore recipe or to skip the recipe all together."
  (format *error-output* "~&Error! ~s does not exist~% Enter a new filename or press return to skip this meal.~%"
          (file-error-pathname condition))
  (format *query-io* "~&> ")
  (let ((user-input (read *query-io*)))
    (if (str:empty? user-input)
        (invoke-restart 'return-value :skipped)
        (invoke-restart 'retry-filename user-input))))


(defmacro hera (&body body)
  (with-gensyms (mealplan)
    `(let ((,mealplan (make-mealplan)))
       (handler-bind ((file-error #'deal-with-file-error))
         ,@(mappend (fn (day-list) (transform-day day-list mealplan))
                    body))
       (mealplan-add-to-apple-notes
        ,mealplan ,*apple-notes-id*)
       (mealplan-add-to-omnifocus
        ,mealplan ,*omnifocus-project*)
       (append-list-to-file ,*tmp-file* (collect-ingredients ,mealplan))
       (format t "~&Edit ~a and press Enter once done~%> " ,*tmp-file*)
       (read-line)
       (mapc (fn (item) (add-item-to-reminders item ,*reminders-list*))
             - (read-list-from-file ,*tmp-file*)))))
