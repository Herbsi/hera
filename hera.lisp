;;;; hera.lisp

(in-package #:hera)

(defun transform-day (day-list mealplan)
  "Processes a day-list like (<day> (<meal> args)+)

into a list of (add-meal (make-meal <meal> args) <day>) function calls."
  (let ((day (make-keyword (first day-list))))
    (mapcar (fn (meal-list) (transform-meal meal-list mealplan day))
            (rest day-list))))


(defun transform-meal (meal-list mealplan day)
  "Transform one (<meal> args) list into a

(add-meal (make-meal <meal> args) mealplan day) list for adding
the meal to the mealplan."
  `(add-meal (make-meal ,@meal-list) ,mealplan ,day))


(defmacro hera (&body body)
  (with-gensyms (mealplan)
    `(let ((,mealplan (make-mealplan)))
       (progn
         ,@(mappend (fn (day-list) (transform-day day-list mealplan))
                   body)))))

(hera
  (Monday
   ("Spätzlepfanne mit Rosenkohl")
   ("Kuchen" :kind :bake))
  (Tuesday
   ("Mehr Kuchen" :kind :bake)))
