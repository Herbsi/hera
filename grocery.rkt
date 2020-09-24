#!/usr/bin/env racket

#lang racket/base

(require "lib.rkt")
(require "omnifocus.rkt")

(define mealplan (string->path "/Users/herwig/Notes/Meal Plan.md"))
(define ingredients-tmp (string->path "/tmp/ingredients"))

(let ([ck-days (mealplan->cook-days mealplan)])
  (for-each (lambda (day)
              (let ([of-args (make-of-arg-list day)]
                    [ingredients (cook-day->ingredients day)])

                  ;; Add Tasks to OF and write ingredients to file
                  (for-each (lambda (arg-set)
                              (apply add-meal-to-omnifocus arg-set))
                            of-args)
                  (write-list-to-file ingredients ingredients-tmp)))
              ck-days)
    (displayln "Added Cooking Tasks to Omnifocus")
    (displayln (format "Ingredients successfully written to ~a" ingredients-tmp))
    (displayln "Edit the Ingredients List and press enter once you're done.")
    (display "> ")
    (read-line)

    ;; Read ingredients back in and add to Reminders
    (for-each (lambda (ingredient)
                (add-item-to-reminders ingredient))
              (read-list-from-file ingredients-tmp))
    (displayln "Added Ingredients to Reminders."))
