#lang racket

(require "../web-funs.rkt"
         #;"../device-descriptions.rkt"
         rackunit
         racket/date
         plot)

(define-logger sodec)

(define HOST "calpolysolardecathlon.org")
(define PORT 3000)

(define SECONDS-IN-DAY 86400)

;; all old broken
#;(
;; FIXME BROKEN
#;(check-equal?
 (remote-call/get
  HOST PORT
  (sodec-url "count-events-in-range"
             `((device "s-elec-used-dining-receps-1")
               (start 0)
               (end ,(find-seconds 0 0 0 1 1 2015)))))
 0)



(define epoch (find-seconds 0 0 0 1 1 2015))
;; if this code runs past the year 2100 we're in trouble:
(define end (find-seconds 0 0 0 1 1 2016))
(define total-seconds (- end epoch))

(define (events-in-division i)
  (define start (round (+ epoch (* i division-seconds))))
  (define end (round (+ start division-seconds)))
  (remote-call/get
   HOST PORT
   (sodec-url "count-events-in-range"
              `((device "s-elec-used-dining-receps-1")
                (start ,start)
                (end ,end)))))


(define division-seconds 86400)
(define divisions (floor (/ (- end epoch) division-seconds)))

;; running this for timing
(for/list ([i (in-range divisions)])
           (printf "i: ~a\n" i)
           (vector i (events-in-division i))))
