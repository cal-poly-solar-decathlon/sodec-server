#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json
         "../device-table.rkt"
         "../web-funs.rkt")

(define-logger sodec)

(define HOST 
  ;; test locally:
  "localhost"
  #;"129.65.138.226"
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
(define (bed-events-in-last-hour)
  (define ts (get-timestamp))
  (gett "events-in-range" `((device "s-temp-bed")
                            (start ,(- ts 3600))
                            (end ,ts))))


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


(run-tests
(test-suite
 "racket server"
 (let ()


   (test-equal? "ping" (gett "ping" #f) "alive")

   (test-case
    "timestamp"
    (check-match (gett "timestamp" #f)
                 (hash-table ('timestamp (? time-near-now? n)))))

   ;; this is kind of meaningless for now...
     (test-case
    "list-devices"
    ;; LIST DEVICES
    (check-equal?
     (gett "list-old-device-ids" #f)
     all-ids))


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

   (test-equal? "no-latest-event"
                (gett "latest-event" '((measurement "temperature")
                                       (device "testing_empty")))
                "no events")

   (test-not-exn
    "old-style-device" 
    (lambda () (gett "latest-event" '((device "s-elec-used-laundry")))))

   ;; new style of electric devices... anything goes!
   (test-case
    "illegal electrical device name"
    (check-match
     (remote-call/get/core HOST PORT (sodec-url "latest-event" '((measurement "electric_power")
                                                                 (device "device with spaces"))))
     (list (regexp #px"^HTTP/1.1 400")
           _1
           (? (port-containing "device with spaces") _3))))

   (test-equal?
    "made-up device"
    (gett "latest-event" '((measurement "electric_power")
                           (device "bogus_device")))
    "no events")
   


   ;; think this endpoint may be going away...
   #;(test-case
    "events-in-empty-range"
    (check-equal?
     (gett "events-in-range"
           '((device s-temp-bed)
             (start 0)
             (end 0)))
     "no events"))
   ;; may be going away...
   #;(test-case
    "too long range for events-in-range"  
    
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
   ;; may be going away...
   #;(test-case
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

   ;; try with electrical ones
   (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   HOST PORT
                   (sodec-url "record-reading" '((device s-elec-used-air-handler-recep)))
                   #"{\"status\":7772387,\"secret\":\"$a8Es#crB469\"}")

                  "okay"))
   

   ;; test sending with urlencoding
   (test-case
    "record-reading with form-urlencoded"
    (check-equal?
     (remote-call/post
      HOST PORT
      (sodec-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=261&secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     "okay"))

   (test-case
    "record-reading with form-urlencoded negative #"
    (check-equal?
     (remote-call/post
      HOST PORT
      (sodec-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=-261&secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     "okay"))
   
   (test-case
    "record-reading with form-urlencoded bad number"
    (check-match
     (remote-call/post/core
      HOST PORT
      (sodec-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=26eee1&secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     (list (regexp #"^HTTP/1.1 400 ")
           _2 _3)))

   (test-case
    "record-reading with form-urlencoded bad alist"
    (check-match
     (remote-call/post/core
      HOST PORT
      (sodec-url "record-reading" '((device s-temp-testing-blackhole)))
      #"status=21&=secret=$a8Es#crB469"
      #:content-type #"application/x-www-form-urlencoded")
     (list (regexp #"^HTTP/1.1 400 ")
           _2 _3)))
   
)))

;; this test suite ensures that the server is receiving
;; data in the expected way from downstream sensors
(run-tests
 (test-suite
  "racket server recording data"
  (let ()
    
    (define ((port-containing str) port)
      (regexp-match (regexp-quote str) port))
    
    (test-case
     "latest-living-room-event"
     (check-match
      (gett "latest-event" '((device s-temp-lr)))
      (? exact-integer? n)))

    
    ;; ignore the occupancy, temp, and ambient light devices:
    (define (ignored-name n)
      (or (regexp-match #px"^s-temp-testing-" n)
          (regexp-match #px"^s-hum-testing-" n)
          (regexp-match #px"^s-amb-" n)
          (regexp-match #px"^s-occ-" n)
          (regexp-match #px"^c-light-" n)))
    
    (for ([device (in-list all-ids)]
          #:when (not (ignored-name device)))
      (test-case
       (~a "latest-event-"device)
       (check-match
        (gett "latest-event" `((device ,device)))
        (? exact-integer? n))))
    
    (test-case
     "count-events-in-range between 10 and 120 bedroom readings in last hour"
     
     (define ((number-in-range a b) n)
       (and (<= a n) (< n b)))
     
     (check-pred (number-in-range 10 120)
                 (let ([ts (get-timestamp)])
                   (gett
                    "count-events-in-range"
                    `((measurement "humidity")
                      (device "bedroom")
                      (start ,(- ts 3600))
                      (end ,ts))))))
    
    
    )))

#;((define ts (get-timestamp))

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

(printf "time since last reading: ~v seconds\n" (- ts last-reading-time)))

