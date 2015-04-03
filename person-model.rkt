#lang typed/racket

;; this will be a model of a person.

;; I think we'll model a person as a priority queue of processes. Processes
;; can change their priorities over time. There will be a "running" process
;; that will control where the person goes.


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
  (sleep seconds))

