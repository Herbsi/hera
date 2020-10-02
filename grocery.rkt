#!/usr/bin/env racket

#lang racket/base

(require "mealplan.rkt")
(require "schedule.rkt")
(require "ingredients.rkt")

(define mealplan-path (string->path "/Users/herwig/Notes/Meal Plan.md"))
(define recipe-location (string->path "/Users/herwig/Notes/Recipes/"))
(define recipe-ext ".md")
(define ingredients-tmp (string->path "/tmp/ingredients"))

(let* ([mealplan (path->mealplan mealplan-path)]
       [schedule (mealplan->schedule mealplan)]
       [ingredients (mealplan->ingredients mealplan recipe-location recipe-ext)])
  (add-schedule-to-omnifocus schedule)
  (displayln "Added Cooking Tasks to Omnifocus")

  (add-schedule-to-notes schedule)
  (displayln "Added Schedule to Notes")

  ;; Write ingrediens to temporary file
  (write-list-to-file ingredients ingredients-tmp)
  (displayln (format "Ingredients successfully written to ~a" ingredients-tmp))
  (displayln "Edit the Ingredients List and press enter once you're done.")
  (display "> ")
  (read-line)

  ;; Read ingredients back in and add to Reminders
  (for ([item (read-list-from-file ingredients-tmp)])
    (add-item-to-reminders item))
  (displayln "Added Ingredients to Reminders."))
