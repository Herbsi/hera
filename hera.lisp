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
  `(add-meal (make-meal ,@meal-list) ,mealplan ,day))


(defun transform-day (day-list mealplan)
  "Processes a day-list like (<day> (<meal> args)+)

into a list of (add-meal (make-meal <meal> args) <day>) function calls."
  (let ((day (make-keyword (first day-list))))
    (mapcar (fn (meal-list) (transform-meal meal-list mealplan day))
            (rest day-list))))


(defmacro hera (&body body)
  (with-gensyms (mealplan)
    `(let ((,mealplan (make-mealplan)))
       ,@(mappend (fn (day-list) (transform-day day-list mealplan))
                  body)
       (mealplan-add-to-apple-notes
        ,mealplan ,*apple-notes-id*)
       (mealplan-add-to-omnifocus
        ,mealplan ,*omnifocus-project*)
       (append-list-to-file ,*tmp-file*
        (collect-ingredients ,mealplan))
       (format t "~&Edit ~a and press Enter once done~%> " ,*tmp-file*)
       (read-line)
       (mapc (fn (item) (add-item-to-reminders item ,*reminders-list*))
             (read-list-from-file ,*tmp-file*)))))

