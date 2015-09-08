#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json
         "../device-descriptions.rkt"
         "../web-funs.rkt")

(define HOST 
  ;; test locally:
  "localhost"
  #;"calpolysolardecathlon.org"
  #;"192.168.2.3")

(define PORT
  #;8080
  3000)

(define (gett . args)
  (remote-call/get HOST PORT (apply sodec-url args)))

(define (get-timestamp)
  (hash-ref (gett "timestamp" #f) 'timestamp))


;; events in last hour on the "s-temp-bed" device
(define (events-in-last-hour)
  (define ts (get-timestamp))
  (gett "events-in-range" `((device "s-temp-bed")
                            (start ,(- ts 3600))
                            (end ,ts))))


;; ping check returns wrong result
;; no events wrong in both places

(define (string-or-null? s)
  (or (eq? s 'null)
      (string? s)))



(define ts (get-timestamp))
(current-seconds)

(define june-one (find-seconds 0 0 0 1 6 2015))

(printf "june-one: ~v\n" june-one)
(printf "one hour later: ~v\n" (+ 3600 june-one))

(time
 (gett "count-events-in-range"
       `((device s-temp-bed)
         (start ,june-one)
         (end ,(+ june-one 3600)))))

;; took 35 seconds on existing setup (rpi2, mysql, db with 7M entries)

#;(define last-hour-jsexpr
  (gett "events-in-range" `((device s-temp-bed)
                            (start ,(- ts 3600))
                            (end ,ts))))

#;(printf "number of readings in the last hour: ~v\n"
        (add1 (length (hash-ref last-hour-jsexpr 'seriesData))))

#;(printf "number of readings in the last hour: ~v\n"
        (gett "count-events-in-range"
              `((device s-temp-bed)
                (start ,(- ts 3600))
                (end ,ts))))

