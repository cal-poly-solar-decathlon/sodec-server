#lang racket

(require "../web-funs.rkt"
         "../device-descriptions.rkt"
         rackunit
         racket/date
         plot)

(define-logger sodec)

(define HOST "calpolysolardecathlon.org")
(define PORT 8080)

(define SECONDS-IN-DAY 86400)


(check-equal?
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

#;(plot
 (points (for/list ([i (in-range divisions)])
           (printf "i: ~a\n" i)
           (vector i (events-in-division i)))))

;; the number of readings from this device in the last 24 hours
(define (device-readings-in-last-day device)
  (define now (current-seconds))
  (define one-day-ago (- now SECONDS-IN-DAY))
  (remote-call/get
   HOST PORT
   (sodec-url "count-events-in-range"
              `((device ,device)
                (start ,one-day-ago)
                (end ,now)))))

;; the expected number of seconds between electrical device readings
(define ELEC-DEVICE-INTERVAL 15)
;; .... temperature and humidity readings
(define TEMP/HUM-DEVICE-INTERVAL 60)

(define expected-elec-device-readings
  (/ SECONDS-IN-DAY ELEC-DEVICE-INTERVAL))
(define expected-temp/hum-device-readings
  (/ SECONDS-IN-DAY TEMP/HUM-DEVICE-INTERVAL))

;; device-alive? : has this device produced about the expected
;; number of readings over the past day?
(define (device-alive? device)
  (log-sodec-debug "checking readings from device: ~a" device)
  
  (match device
    [(or (regexp #px"^s-elec-used-")
         (regexp #px"^s-elec-gen-"))
     (define num-readings
       (device-readings-in-last-day device))
     (device-performance num-readings
                         expected-elec-device-readings)]
    [(or (regexp #px"^s-temp-")
         (regexp #px"^s-hum-"))
     (define num-readings
       (device-readings-in-last-day device))
     (device-performance num-readings
                         expected-temp/hum-device-readings)]
    [(regexp #px"^s-light-")
     (light-last-reading device)]
    [(or (regexp #px"^s-occ-")
         (regexp #px"^s-amb-")
         (regexp #px"^c-light-")
         (regexp #px"^s-temp-testing-"))
     '(ignored)]
    [other
     (printf "ignoring device: ~v\n" device)]))

;; return a list indicating the number of seconds since the
;; last update
(define (light-last-reading device)
  (define response
    (remote-call/get
     HOST PORT
     (sodec-url "latest-event" `((device ,device)))))
  (cond [(equal? response "no events")
         `(never-used)]
        [else
         (define reading-timestamp (hash-ref response 'timestamp))
         `(idle-for-seconds ,(- (current-seconds) reading-timestamp))]))

;; given a number of readings and an expected number of
;; readings, return a list indicating whether the number
;; of readings is unexpectedly low or high
(define (device-performance num-readings expected-num-readings)
  (define frac (/ num-readings expected-num-readings))
  (cond [(< frac 0.9)
         (list 'too-low num-readings frac)]
        [(< frac 1.1)
         (list 'as-expected num-readings frac)]
        [else
         (list 'too-high num-readings frac)]))

(for/list ([device (in-list device-strs)])
  (list device (device-alive? device)))



