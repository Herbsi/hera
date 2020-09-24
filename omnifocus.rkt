#lang racket/base

(require racket/list)
(require racket/sequence)
(require racket/system)

(require gregor)

(require "lib.rkt")

(provide add-meal-to-omnifocus
         make-of-arg-list)

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

(define (add-meal-to-omnifocus meal due-day due-time)
  "Adds a Cook `meal` task, due on `due-day` at `due-time` to Omnifocus"
  ;; TODO remove absolute path
  (system (format "osascript \"/Users/herwig/Automations/Grocery/Add Meal to Omnifocus.scpt\" \"~a\" \"~a ~a\" "
                  meal due-day due-time)))
