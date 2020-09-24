#lang racket/base

(require racket/list)
(require racket/sequence)
(require racket/string)
(require racket/system)

(require gregor)

(provide mealplan->cook-days
         (rename-out [ck-day->ingredients cook-day->ingredients])
         make-of-arg-list
         write-list-to-file
         read-list-from-file
         add-item-to-reminders
         add-meal-to-omnifocus
         (struct-out ck-day))

(struct ck-day
  (weekday meals)
  #:transparent)

(define (mealplan->cook-days path)
  "Takes the meal plan file and returns a list of ck-day structs"
  (define (process-day str)
    (let* ([str (bytes->string/utf-8 str)]
           [day (car (regexp-match #px"(?<=# )\\w+(?= #)" str))]
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
                        (car
                         (regexp-match #px"(?<=Ingredients\n)(?m:\\*.+\\\n)+"
                                       content)))))
  ;; TODO remove this absolute path
  (let ([recipe-root (string->path "/Users/herwig/Notes/Recipes")]
        [recipe-ext ".md"])
    (call-with-input-file
      (build-path recipe-root (string-append recipe recipe-ext))
      content->ingredients)))

(define (ck-day->ingredients ck-day)
  (append-map recipe->ingredients (ck-day-meals ck-day)))


(define (make-of-arg-list ck-day)
  "Takes a cook-day struct and returns a list of argument lists fit for

add-meal-to-omnifocus"
  (define weekday-number #hash(("Sunday" . 0)
                               ("Monday" . 1)
                               ("Tuesday" . 2)
                               ("Wednesday" . 3)
                               ("Thursday" . 4)
                               ("Friday" . 5)
                               ("Saturday" . 6)))
  (define (calculate-day-string weekday)
    ;; Returns an ISO-8601 formatted date string of the next weekday
    (let* ([today (today)]
           
           [days-to-add (modulo (- (hash-ref weekday-number weekday)
                                             (->wday today))
                                          7)])
      ;; today is not an upcoming cooking-day, next week is
      (~t (+days today (if (zero? days-to-add) 7 days-to-add))
          "yyyy-MM-dd")))

  ;; TODO better variable names
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
   ;; TODO remove absolute path
   (format "osascript \"/Users/herwig/Automations/Grocery/Add Item to Reminders.scpt\" \"~a\" \"~a\"" lst item)))


(define (add-meal-to-omnifocus meal due-day due-time)
  "Adds a Cook `meal` task, due on `due-day` at `due-time` to Omnifocus"
  ;; TODO remove absolute path
  (system (format "osascript \"/Users/herwig/Automations/Grocery/Add Meal to Omnifocus.scpt\" \"~a\" \"~a ~a\" "
                  meal due-day due-time)))
