#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json
         "../device-descriptions.rkt"
         "../web-funs.rkt")

(define-logger sodec)

(define HOST 
  ;; test locally:
  #;"localhost"
  "calpolysolardecathlon.org"
  #;"192.168.2.3")

(define PORT
  8080
  #;3000)

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



(run-tests
(test-suite
 "racket evaluator tests"
 (let ()


   (test-equal? "ping" (gett "ping" #f) "alive")

   (test-case
    "timestamp"
    (match (gett "timestamp" #f)
      [(hash-table ('timestamp (? number? n)))
       (check > n (find-seconds 0 0 0 1 1 2014))
       (check < n (find-seconds 0 0 0 1 1 2016))]
      [other 
       (fail "timestamp shape")]))
   
   (define ((port-containing str) port)
     (regexp-match (regexp-quote str) port))

   ;; bogus endpoint
   (test-case
    "404s"
    ;; simple 404:
    (check-match (remote-call/get/core HOST PORT (sodec-url "blothints" #f))
                 (list (regexp #"^HTTP/1.1 404")
                       _1
                       (? (port-containing "blothints") _3)))

    ;; latest event
    
    ;; near miss on the device name:
    (check-match (remote-call/get/core HOST PORT (sodec-url "latest-event"
                                                            `((device uhnoth))))
                 (list (regexp #px"^HTTP/1.1 40[04]")
                       _1
                       (? (port-containing "uhnoth") _3))))

   (test-case
    "list-devices"
    ;; LIST DEVICES
    (check-pred (lambda (devlist)
                  (and (list? devlist)
                       (for/and ([ht (in-list devlist)])
                         (match ht
                           [(hash-table ('device (? string? dev))
                                        ('description (? string-or-null? descn)))
                            #t]
                           [other #f]))))
                (gett "list-devices" #f)))

   (gett "list-devices" #f)

   (define listed-devices
     (map (lambda (ht)
            (list (hash-ref ht 'device)
                  (hash-ref ht 'description)))
          (gett "list-devices" #f)))

   (test-case
    "list-devices-descriptions"
   ;; check that all of the listed devices are present:
   (for ([dd-pair (in-list dd-pairs)])
     (define device-name (first dd-pair))
     (check-not-exn (lambda ()
                      (dict-ref listed-devices device-name)))
     (check-equal? (dict-ref listed-devices device-name)
                   (list (second dd-pair)))))

   
   
   (test-equal? "empty-latest-events"
                (gett "latest-event" '((device s-temp-testing-empty)))
                "no events")

   (test-case
    "latest-living-room-event"
   (check-match
    (gett "latest-event" '((device s-temp-lr)))
    (hash-table ('timestamp (? number? n))
                  ('device-id "s-temp-lr")
                  ('status (? number? s)))))

   ;; ignore the occupancy, temp, and ambient light devices:
   (define (ignored-name n)
     (or (regexp-match #px"^s-temp-testing-" n)
         (regexp-match #px"^s-amb-" n)
         (regexp-match #px"^s-occ-" n)
         (regexp-match #px"^c-light-" n)))
   
   (for ([device (in-list device-strs)]
         #:when (not (ignored-name device)))
     (test-case
      (~a "latest-event-"device)
      (check-match
       (gett "latest-event" `((device ,device)))
       (hash-table ('timestamp (? number? n))
                   ('device-id device)
                   ('status (? number? s))))))
   

   (test-case
    "events-in-empty-range"
    (check-equal?
     (gett "events-in-range"
           '((device s-temp-bed)
             (start 0)
             (end 0)))
     "no events"))
   
   (test-case
    "events-in-range"
    (check-match (events-in-last-hour)
                 (hash-table ('baseTimestamp (? number? _1))
                             ('baseStatus (? values _2))
                             ('seriesData (? values _3))))
    
    ;; more than a day of data:
    (check-match (remote-call/get/core
                  HOST PORT
                  (sodec-url "events-in-range"
                             `((device s-temp-bed)
                               (start 0)
                               (end 100000))))
                 (list #"HTTP/1.1 400 range too long"
                       _2
                       _3)))

   (test-case
    "count-events-in-range bad args"
    (check-match (remote-call/get/core
                  HOST PORT
                  (sodec-url "count-events-in-range" #f))
                 (list #"HTTP/1.1 404 wrong query fields" _2 _3))
    
    (check-match (remote-call/get/core
                  HOST PORT
                  (sodec-url "count-events-in-range" '((device foo)
                                                       (start 0)
                                                       (end 1))))
                 (list #"HTTP/1.1 404 wrong query fields"  _2 _3)))
   
   (test-case
    "count-events-in-range"
    
    (define ((number-in-range a b) n)
      (and (<= a n) (< n b)))
    
    (check-pred (number-in-range 10 722)
                (let ([ts (get-timestamp)])
                  (gett
                   "count-events-in-range" `((device s-temp-lr)
                                             (start ,(- ts 3600))
                                             (end ,ts))))))

   ;; RECORDING READINGS

   (test-case
    "record-reading-404"
    (check-match (remote-call/post/core
                  HOST PORT
                  (sodec-url "record-reading" `((device uhnoth)))
                  #"1234")
                 (list (regexp #px#"^HTTP/1.1 404")
                       _1
                       (? (port-containing "uhnoth") _3))))

   (test-case
    "record-reading-bad-json"
    
    (check-match (remote-call/post/core
                  HOST PORT
                  (sodec-url "record-reading" '((device s-temp-lr)))
                  #"abcd")
                 (list #"HTTP/1.1 400 bad JSON in POST"
                       _2
                       _3)))
   
   (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   HOST PORT
                   (sodec-url "record-reading" '((device s-temp-testing-blackhole)))
                   #"{\"status\":7772387,\"secret\":\"$a8Es#crB469\"}")
                  "okay"))

   
)))

(define ts (get-timestamp))

;; this is getting a bit nasty in the string-append region...
(define last-hour-jsexpr
  (gett "events-in-range" `((device s-temp-bed)
                            (start ,(- ts 3600))
                            (end ,ts))))

(printf "number of readings in the last hour: ~v\n"
        (add1 (length (hash-ref last-hour-jsexpr 'seriesData))))

(printf "number of readings in the last hour: ~v\n"
        (gett "count-events-in-range"
              `((device s-temp-bed)
                (start ,(- ts 3600))
                (end ,ts))))

(define jsexpr-len
  (bytes-length
   (jsexpr->bytes last-hour-jsexpr)))

(printf "bytes per reading: ~v\n" (/ jsexpr-len 
                                     (add1 (length 
                                            (hash-ref last-hour-jsexpr
                                                      'seriesData)))))

(define last-reading
  (gett "latest-event" '((device s-temp-bed))))

(define last-reading-time (hash-ref last-reading 'timestamp))

(printf "time since last reading: ~v seconds\n" (- ts last-reading-time))

