#lang racket

(require gregor)

(require "mealplan.rkt")

(provide mealplan->schedule
         add-schedule-to-omnifocus)

(struct schedule
  (cooking-tasks))

(struct cooking-task
  (date time meal))

(define (mealplan->schedule mp)
  (define (calc-date-string weekday)
    ;; Returns an ISO-8601 formatted date string of the next weekday
    (let* ([weekday-number #hasheq((Sunday . 0)
                                   (Monday . 1)
                                   (Tuesday . 2)
                                   (Wednesday . 3)
                                   (Thursday . 4)
                                   (Friday . 5)
                                   (Saturday . 6))]
           [today (today)]
           [days-to-add (modulo (- (hash-ref weekday-number weekday)
                                   (->wday today))
                                7)])
      ;; today is not an upcoming cooking-day, next week is
      (~t (+days today (if (zero? days-to-add) 7 days-to-add))
          "yyyy-MM-dd")))
  (schedule
   (apply append
          (for/list ([(wday meals) (mealplan-dict mp)])
            (let ([date (calc-date-string wday)])
              (for/list ([meal meals]
                         [time (sequence-append '("12:00") (in-cycle '("17:00")))])
                (cooking-task date time meal)))))))

(define (add-cooking-task-to-omnifocus ck-task)
  "Adds a Cook `meal` task, due on `due-day` at `due-time` to Omnifocus"
  (system (format "osascript \"Add Meal to Omnifocus.scpt\" \"~a\" \"~a ~a\" "
                  (cooking-task-meal ck-task)
                  (cooking-task-date ck-task)
                  (cooking-task-time ck-task))))

(define (add-schedule-to-omnifocus schedule)
  (for ([ck-task (schedule-cooking-tasks schedule)])
    (add-cooking-task-to-omnifocus ck-task)))
