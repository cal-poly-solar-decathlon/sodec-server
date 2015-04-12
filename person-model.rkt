#lang typed/racket

(require "generate-sensor-names.rkt")
;; this will be a model of a person.

(require rackunit)

;; a mapping from hour and minute to seconds

(define (t h m)
  (+ (* h HOUR-SECONDS)
     (* m MINUTE-SECONDS)))

(define HOUR-SECONDS 3600)
(define MINUTE-SECONDS 60)

(check-equal? (t 8 19)
              (+ (* 8 3600) (* 19 60)))

;; a simple schedule-based model for lighting.
;; a schedule is a list of event-begins. An event
;; is presumed to continue until the beginning
;; of the next event

;; an event-begin is a list containing an activity
;; name and a number of seconds (since midnight)
(define example-kid-schedule
  `((home ,(t 7 00))
    (breakfast ,(t 7 15))
    (school ,(t 7 30))
    (home ,(3 30))
    (dinner ,(6 00))
    (home ,(7 00))
    (sleep ,(8 30))))

;; initially, each event-begin generates
;; a single room-move-event

;; an event is a list containing time (in seconds
;; since midnight), person name, and a list of light
;; names currently being used

(define example-event (list (t 9 45) 'alexander '(s-light-chandelier-1B)))


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

