#lang racket/base

(require racket/list)
(require racket/system)

(require anaphoric)

(require "mealplan.rkt")

(provide mealplan->ingredients
         add-item-to-reminders
         write-list-to-file
         read-list-from-file)

(define (mealplan->recipes mp)
  (remove-duplicates (apply append
                            (hash-values (mealplan-dict mp)))))


(define (mealplan->ingredients mp recipe-location recipe-ext)
  (append-map (lambda (item)
                (recipe->ingredients item recipe-location recipe-ext))
              (mealplan->recipes mp)))


(define (recipe->ingredients meal recipe-location recipe-ext)
  "Takes a single recipe and returns a list with its ingredients"
  (define (content->ingredients content)
    (map bytes->string/utf-8
         ;; extracts the ingredients, one ingr per list-element
         (regexp-match* #rx"(?m:(?<=\\* ).+)"
                        (cadr
                         ;; extracts the block with only the relevant ingredients
                         (regexp-match #px"(?<=## )Ingredients(?: ##)?\\S*\n+((?m:\\*.+\\\n)+)"
                                       content)))))
  (let ([result null])
    (let iter ([meal meal])
      (for ([item (call-with-input-file
                      (build-path recipe-location (string-append meal recipe-ext))
                    content->ingredients)])
        (aif (regexp-match #px"[[]{2}(.*?)[]]{2}" item)
             (iter (cadr it))
             (set! result (cons item result)))))
    result))


(define (write-list-to-file lst path)
  "Writes lst to path, separated by newlines"
  (call-with-output-file path
    #:exists 'truncate
    (lambda (out)
      (for ([item lst])
        (displayln item out)))))


(define (read-list-from-file path)
  "Reads a list from path, one item per line"
  (call-with-input-file path
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))


(define (add-item-to-reminders item (lst "Grocery"))
  (system
   (format "osascript \"Add Item to Reminders.scpt\" \"~a\" \"~a\"" lst item)))
