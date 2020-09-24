#!/usr/bin/env racket

#lang racket/base

(require racket/list)
(require racket/sequence)
(require racket/string)
(require racket/system)

(require gregor)

(define mealplan (string->path "/Users/herwig/Notes/Meal Plan.md"))
(define ingredients-tmp (string->path "/tmp/ingredients"))
(define recipe-root (string->path "/Users/herwig/Notes/Recipes"))
(define recipe-ext ".md")

(struct ck-day
  (weekday meals)
  #:transparent)


(define (mealplan->cook-days path)
  "Takes the meal plan file and returns a list of ck-day structs"
  (define (process-day str)
    (let* ([str (bytes->string/utf-8 str)]
           [day (first (regexp-match #px"(?<=# )\\w+(?= #)" str))]
           [meals (regexp-match* #px"(?m:(?<=- ).+)" str)])
      (ck-day day meals)))
  (call-with-input-file
    path
    (lambda (in)
      (map process-day
           (regexp-match* #px"# \\w+ #\n(?:- .+?\n)+" in)))))


(define (recipe->ingredients recipe)
  "Takes a single recipe and returns a list with its ingredients"
  (define (content->ingredients content)
    (map bytes->string/utf-8
         (regexp-match* #rx"(?m:(?<=\\* ).+)"
                        (first
                         (regexp-match #px"(?<=Ingredients\n)(?m:\\*.+\\\n)+"
                                       content)))))
  (call-with-input-file
    (build-path recipe-root (string-append recipe recipe-ext))
    content->ingredients))


(define (make-of-arg-list ck-day)
  "Takes a cook-day struct and returns a list of argument lists fit for

add-meal-to-omnifocus"
  (define (calculate-day-string weekday)
    ;; Returns an ISO-8601 formatted date string of the next weekday
    (let* ([today (today)]
           [weekday-number #hash(("Sunday" . 0)
                                 ("Monday" . 1)
                                 ("Tuesday" . 2)
                                 ("Wednesday" . 3)
                                 ("Thursday" . 4)
                                 ("Friday" . 5)
                                 ("Saturday" . 6))]
           [days-to-add (modulo (- (hash-ref weekday-number weekday)
                                             (->wday today))
                                          7)])
      ;; today is not an upcoming cooking-day, next week is
      (~t (+days today (if (zero? days-to-add) 7 days-to-add))
          "yyyy-MM-dd")))
  (let ([day (calculate-day-string (ck-day-weekday ck-day))])
    (for/list ([meal (ck-day-meals ck-day)]
               [due-day (in-cycle `(,day))]
               [due-time (sequence-append '("12:00") (in-cycle '("17:00")))])
      (list meal due-day due-time))))


(define (write-list-to-file lst path)
  "Writes lst to path, separated by newlines"
  (call-with-output-file path
    #:exists 'truncate
    (lambda (out)
      (for-each (lambda (item) (display item out) (display "\n" out))
                lst))))


(define (read-list-from-file path)
  "Reads a list from path, one item per line"
  (call-with-input-file path
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))


(define (add-item-to-reminders item (lst "Grocery"))
  (system
   (format "osascript \"/Users/herwig/Automations/Grocery/Add Item to Reminders.scpt\" \"~a\" \"~a\"" lst item)))


(define (add-meal-to-omnifocus meal due-day due-time)
  "Adds a Cook `meal` task, due on `due-day` at `due-time` to Omnifocus"
  (system (format "osascript \"/Users/herwig/Automations/Grocery/Add Meal to Omnifocus.scpt\" \"~a\" \"~a ~a\" "
                  meal due-day due-time)))


(define (main)
  (let ([ck-days (mealplan->cook-days mealplan)])
    (for-each (lambda (day)
                (let ([of-args (make-of-arg-list day)]
                      [ingredients (append-map recipe->ingredients (ck-day-meals day))])
                  ;; Add Tasks to OF and write ingredients to file
                  (for-each (lambda (arg-set)
                              (apply add-meal-to-omnifocus arg-set))
                            of-args)
                  (write-list-to-file ingredients ingredients-tmp)))
              ck-days)
    (display "Added Cooking Tasks to Omnifocus\n")
    (display (format "Ingredients successfully written to ~a\n" ingredients-tmp))
    (display "Edit the Ingredients List and press enter once you're done.\n> ")
    (read-line)
    ;; Read ingredients back in and add to reminders
    (for-each (lambda (ingredient) (add-item-to-reminders ingredient))
              (read-list-from-file ingredients-tmp))
    (display "Added Ingredients to Reminders")))

(main)
