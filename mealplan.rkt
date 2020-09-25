#lang racket/base

(provide path->mealplan
         (struct-out mealplan))

(struct mealplan
  (dict))

(define (path->mealplan path)
  "Takes path to a “mealplan file” and returns a mealplan struct"
  (define (process-day str)
    (let* ([str (bytes->string/utf-8 str)]
           [day (string->symbol
                 (car (regexp-match #px"(?<=# )\\w+(?= #)" str)))]
           [meals (regexp-match* #px"(?m:(?<=- ).+)" str)])
      (cons day meals)))
  (call-with-input-file
      path
    (lambda (in)
      (mealplan
       (make-hasheq
        (map process-day
             (regexp-match* #px"# \\w+ #\n(?:- .+?\n)+" in)))))))
