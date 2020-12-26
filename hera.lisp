;;;; hera.lisp

(in-package #:hera)

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
       (add-mp-to-apple-notes ,mealplan "x-coredata://F93C847E-0906-4FFC-881E-472B69BB9B9C/ICNote/p231"))))

(hera
  (Monday
   ("Spätzlepfanne mit Rosenkohl.md")
   ("Schoko Gugelhupf.md" :kind :bake :apple-notes nil))
  (Tuesday
   ("Schoko-Mandel Kuchen.md" :kind :bake)))
