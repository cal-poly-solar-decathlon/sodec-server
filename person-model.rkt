#lang typed/racket

(require "generate-sensor-names.rkt")
;; this will be a model of a person.

;; Okay, I keep changing my mind. Let's start with a simple schedule.
;; A schedule is a list of times (of day) and activities, representing
;; that activity, beginning at the given time and continuing until the next
;; time.

;; a time of day is represented as a natural number in [0 .. 86400)

;; given a time in hours & minutes, return a number of seconds:
(: t (Natural Natural -> Natural))
(define (t hours minutes)
  (+ (* 3600 hours) (* 60 minutes)))

;; an activity is a symbol

(define example-kid-schedule
  `((,(t 8 30) 'breakfast)
    (,(t 9 00) 'school)
    (,(t 15 00) 'home)
    (,(t 18 00) 'dinner)
    (,(t 19 00) 'home)
    (,(t 21 00) 'sleep)))

;; a light is a symbol in light-names



;; given an activity, return a list of lights on for that activity
(: light-map (Listof (List Symbol (Listof Symbol))))
(define light-map
  '((breakfast (s-light-kitchen-uplight-3A
                s-light-pendant-bar-lights-3C))
    (school ())
    (home (s-light-flexspace-uplight-5A
           s-light-tv-light-2A))
    (dinner (s-light-chandelier-1B))
    (sleep ())))

;; I think we'll model a person as a priority queue of processes. Processes
;; can change their priorities over time. There will be a "running" process
;; that will control where the person goes.

#;(
(define bathroom-process
  (sequence (go-to 'bathroom)
            (stay-for (approximately (minutes 5)))))

(define bathroom-priority
  (cond [(running? bathroom-process) 100]
        [else (define t (time-since-process-completed bathroom-process))
              (cond [(< t 30) 0]
                    [(< t 60) 5]
                    [(< t 90) 10]
                    [(< t 120) 20]
                    [(< t 180) 40]
                    [else 80])]))

;; go to a particular room
(define (go-to room)
  (printf "going to ~v\n" room))

;; work on a process for some number of seconds.
(define (stay-for seconds)
  (sleep seconds)))

