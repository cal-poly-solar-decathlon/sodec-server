#lang racket/base

;; this file is just for interactive querying--it's easier than constructing URLs by hand.
(require net/url
         net/head
         racket/date
         racket/block
         racket/contract
         racket/format
         json
         "../device-table.rkt"
         "../web-funs.rkt")

(define-logger sodec)

(module* test racket/base)

(define HOST 
  ;; test locally:
  #;"localhost"
  #;"129.65.138.226"
  "calpolysolardecathlon.org"
  #;"192.168.2.3")

(define PORT
  3000
  #;8080)

(define (gett . args)
  in-list (remote-call/get HOST PORT (apply sodec-url args)))

(define (get-timestamp)
  (hash-ref (gett "timestamp" #f) 'timestamp))

;; expected intervals, in seconds
(define TEMP-HUM-INTERVAL 45)
(define ELEC-INTERVAL 15)

;; status report covers this many trailing seconds
(define CHECK-TIME (* 60 60 24))

(define (check-time-events-count measurement device)
  (define ts (current-seconds))
  (gett "count-events-in-range" `((measurement ,measurement)
                            (device ,device)
                            (start ,(- ts CHECK-TIME))
                            (end ,ts))))

(define temp-hum-readings-expected (/ CHECK-TIME TEMP-HUM-INTERVAL))
(define elec-readings-expected (/ CHECK-TIME ELEC-INTERVAL))

(define (print-status-report)
  (for ([measurement (in-list MEASUREMENT-NAMES)])
    (printf "## ~a\n" measurement)
    (define expected-num-readings
      (cond [(equal? measurement "electric_power") elec-readings-expected]
            [else temp-hum-readings-expected]))
    (for ([device (hash-ref measurement-device-table measurement)]
          #:when (not (regexp-match #px"^testing_" device)))
      (define num-readings (check-time-events-count measurement device))
      (define pct (round
                   (* 100 (/ num-readings expected-num-readings))))
      (define alert (cond [(<= pct 95)
                           (format " : *LOW* ~a%" pct)]
                          [(<= pct 100) ""]
                          [else
                           (format " : *HIGH* ~a%" pct)]))
      (printf "~v : ~v~a\n"
              device
              num-readings
              alert))))

(print-status-report)

(block
 (define ts (current-seconds))
 (define (check-hour-events-count measurement device hours-ago)
   (gett "count-events-in-range" `((measurement ,measurement)
                                   (device ,device)
                                   (start ,(- ts (* hours-ago 3600)))
                                   (end ,(- ts (* (sub1 hours-ago) 3600))))))
 
 (define temp-hum-readings-expected (/ 3600 TEMP-HUM-INTERVAL))
 (define elec-readings-expected (/ 3600 ELEC-INTERVAL))
 
 (define (print-hour-line hours-ago)
   (for ([measurement (in-list MEASUREMENT-NAMES)])
     (define expected-num-readings
       (cond [(equal? measurement "electric_power") elec-readings-expected]
             [else temp-hum-readings-expected]))
     (for ([device (hash-ref measurement-device-table measurement)]
           #:when (not (regexp-match #px"^testing_" device)))
       (define num-readings (check-hour-events-count measurement device hours-ago))
       (define pct (round
                    (* 100 (/ num-readings expected-num-readings))))
       (define status-char
         (cond [(<= pct 2) "X"]
               [(<= pct 75) "d"]
               [(<= pct 80)  "c"]
               [(<= pct 90) "b"]
               [(<= pct 95) "a"]
               [else "."]))
       (display status-char)))
   (display "\n"))

 (define LAST-HOURS 5)
 (for ([i LAST-HOURS])
   (print-hour-line (- LAST-HOURS i))))


;; ping check returns wrong result
;; no events wrong in both places

(define (string-or-null? s)
  (or (eq? s 'null)
      (string? s)))


(define (time-near-now? n)
  (and (exact-integer? n)
       (< (abs (- (current-seconds) n)) 100)))

(define ((port-containing str) port)
  (regexp-match (regexp-quote str) port))

(define ((eqto? a) b)
  (equal? a b))



