#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json
         "../generate-sensor-names.rkt"
         "../web-funs.rkt")

(define-logger sodec)

;; given a URL, make a GET request and wait for a response, returning a jsexpr
(define (remote-call/get url-string)
  (results->jsexpr (remote-call/get/core url-string)))


;; given a URL string, return the response code, the first line, the rest
;; of the headers, and the port for the remainder of the body
(define (remote-call/get/core url-string)
  (log-sodec-debug "remote-call/get/core: url-string ~a"
             url-string)
  (response-port->results (get-impure-port (string->url url-string))))



(define l-u 
  ;; test locally:
  "http://localhost:8080"
  ;; test brinckerhoff.org (whatever it points to)
  #;"http://calpolysolardecathlon.org:8080"
  #;"http://192.168.2.3:3000"
  #;"http://calpolysolardecathlon.org:3000")

(define (rel-url str)
  (string-append l-u str))


(define (call-subpath subpath)
  (remote-call/get (rel-url (~a "/srv" subpath))))

(define (call-subpath/post subpath post-bytes)
  (remote-call/post (rel-url (~a "/srv" subpath)) post-bytes))

;; map query associations to a string
(define (query->string assoc)
  (apply
   string-append
   (add-between
    (for/list ([pr (in-list assoc)])
      (match pr
        [(list (? clean-string? from) (? clean-string? to))
         (~a from "=" to)]))
    SEP-STR)))

(define SEP-STR "&")

;; a string with only alphanum and hyphens OR an exact integer OR a symbol
;; whose corresponding string is alphanum & hyphens
(define (clean-string? str)
  (or (and (string? str)
           (regexp-match #px"^[A-Za-z0-9-]*$" str)
           #t)
      (exact-integer? str)
      (and (symbol? str)
           (clean-string? (symbol->string str)))))

(check-equal? (clean-string? "hth-t987") #t)
(check-equal? (clean-string? "hth-t98.7") #f)

(check-equal? (query->string '(("device" "s-temp-bed")
                               ("start" 273)
                               ("end" "29")))
              "device=s-temp-bed&start=273&end=29")


;; formulate a request URL
(define (sodec-url endpoint query)
  (string-append l-u "/srv/" endpoint
                 (cond [query (string-append QUERY-START (query->string query))]
                       [else ""])))

(define QUERY-START "?")

(check-equal? (sodec-url "latest-event"
                        `((device s-temp-bed)))
              (string-append l-u "/srv/latest-event?device=s-temp-bed"))
(check-equal? (sodec-url "latest-event" #f)
              (string-append l-u "/srv/latest-event"))

(define (get-timestamp)
  (hash-ref (remote-call/get (sodec-url "timestamp" #f)) 'timestamp))


;; events in last hour on the "s-temp-bed" device
(define (events-in-last-hour)
  (define ts (get-timestamp))
  ;; this is getting a bit nasty in the string-append region...
  (remote-call/get
   (sodec-url "events-in-range" `(("device" "s-temp-bed")
                                  ("start" ,(- ts 3600))
                                  ("end" ,ts)))))


;; ping check returns wrong result
;; no events wrong in both places

(define (string-or-null? s)
  (or (eq? s 'null)
      (string? s)))

(run-tests
(test-suite
 "racket evaluator tests"
 (let ()


   (test-equal? "ping" (remote-call/get (sodec-url "ping" #f)) "alive")

   (test-case
    "timestamp"
    (match (remote-call/get (sodec-url "timestamp" #f))
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
    (check-match (remote-call/get/core (rel-url "/srv/blothints"))
                 (list 404
                       _1
                       _2
                       _3
                       ;; sigh...
                       #;(? (port-containing "blothints") _3)))

    ;; latest event
    
    ;; near miss on the device name:
    (check-match (remote-call/get/core (sodec-url "latest-event"
                                                  `((device uhnoth))))
                 (list (or 404 400)
                       (regexp #px"^HTTP/1.1 40[04]")
                       _2
                       _3
                       ;; sigh...
                       #;(? (port-containing "uhnoth") _3))))

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
                (remote-call/get (sodec-url "list-devices" #f))))

   (test-case
    "list-devices-right-description"
    ;; LIST DEVICES
    (check-pred (lambda (devlist)
                  (and (list? devlist)
                       (for/or ([ht (in-list devlist)])
                         (match ht
                           [(hash-table ('device "s-temp-out")
                                        ('description "Outside Temperature"))
                            #t]
                           [other #f]))))
                (remote-call/get (sodec-url "list-devices" #f))))
   
   
   (test-equal? "empty-latest-events"
                (remote-call/get (sodec-url "latest-event"
                                            '((device s-temp-testing-empty))))
                "no events")

   (test-case
    "latest-living-room-event"
   (check-match
    (remote-call/get (sodec-url "latest-event" '((device s-temp-lr))))
    (hash-table ('timestamp (? number? n))
                  ('device-id "s-temp-lr")
                  ('status (? number? s)))))

   ;; ignore the occupancy, temp, and ambient light devices:
   (define (ignored-name n)
     (or (regexp-match #px"^s-temp-testing-" (symbol->string n))
         (regexp-match #px"^s-amb-" (symbol->string n))
         (regexp-match #px"^s-occ-" (symbol->string n))))
   
   (for ([device (in-list device-names)]
         #:when (not (ignored-name device)))
     (test-case
      (~a "latest-event-"device)
      (check-match
       (remote-call/get (sodec-url "latest-event" `((device ,device))))
       (hash-table ('timestamp (? number? n))
                   ('device-id device)
                   ('status (? number? s))))))
   

   (test-case
    "events-in-empty-range"
    (check-equal?
     (remote-call/get (sodec-url "events-in-range"
                                 '((device s-temp-bed)
                                   (start 0)
                                   (end 0))))
     "no events"))
   
   (test-case
    "events-in-range"
    (check-match (events-in-last-hour)
                 (hash-table ('baseTimestamp (? number? _1))
                             ('baseStatus (? values _2))
                             ('seriesData (? values _3))))
    
    ;; more than a day of data:
    (check-match (remote-call/get/core
                  (sodec-url "events-in-range"
                             `((device s-temp-bed)
                               (start 0)
                               (end 100000))))
                 (list 400
                       "HTTP/1.1 400 range too long\r"
                       _2
                       _3)))

   (test-case
    "count-events-in-range"
    (check-match (remote-call/get/core
                  (sodec-url "count-events-in-range" #f))
                 (list 404
                       "HTTP/1.1 404 wrong query fields\r" _2 _3))

    (check-match (remote-call/get/core
                  (sodec-url "count-events-in-range" '((device foo)
                                                       (start 0)
                                                       (end 1))))
                 (list 404
                       "HTTP/1.1 404 wrong query fields\r"  _2 _3))

    (define ((number-in-range a b) n)
      (and (<= a n) (< n b)))
    
    (check-pred (number-in-range 10 722)
                (let ([ts (get-timestamp)])
                (remote-call/get
                 (sodec-url "count-events-in-range" `((device s-temp-lr)
                                                      (start ,(- ts 3600))
                                                      (end ,ts)))))))

   ;; RECORDING READINGS

   (test-case
    "record-reading-404"
    (check-match (remote-call/post/core
                  (sodec-url "record-reading" `((device uhnoth)))
                  #"1234")
                 (list (or 404 400)
                       (regexp #px"^HTTP/1.1 40[04]")
                       _2
                       _3
                       ;; sigh...
                       #;(? (port-containing "uhnoth") _3))))

   (test-case
    "record-reading-bad-json"
    
    (check-match (remote-call/post/core
                  (sodec-url "record-reading" '((device s-temp-lr)))
                  #"abcd")
                 (list 400
                       "HTTP/1.1 400 bad JSON in POST\r"
                       _2
                       _3)))
   
   (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   (sodec-url "record-reading" '((device s-temp-testing-blackhole)))
                   #"{\"status\":7772387,\"secret\":\"$a8Es#crB469\"}")
                  "okay"))

      (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   (string-append l-u
                                  "/srv/record-reading?device=s-temp-lr")
                   #"{\"status\":1234,\"secret\":\"$a8Es#crB469\"}")
                  "okay"))

   
)))

(remote-call/get
 (sodec-url "list-devices" #f))

(define ts (get-timestamp))

;; this is getting a bit nasty in the string-append region...
(define last-hour-jsexpr
  (remote-call/get
   (sodec-url "events-in-range" `((device s-temp-bed)
                                  (start ,(- ts 3600))
                                  (end ,ts)))))

(printf "number of readings in the last hour: ~v\n"
        (add1 (length (hash-ref last-hour-jsexpr 'seriesData))))

(printf "number of readings in the last hour: ~v\n"
        (remote-call/get
         (sodec-url "count-events-in-range"
                    `((device s-temp-bed)
                                  (start ,(- ts 3600))
                                  (end ,ts)))))

(define jsexpr-len
  (bytes-length
   (jsexpr->bytes last-hour-jsexpr)))

(printf "bytes per reading: ~v\n" (/ jsexpr-len 
                                     (add1 (length 
                                            (hash-ref last-hour-jsexpr
                                                      'seriesData)))))

(define last-reading
  (remote-call/get (sodec-url "latest-event" '((device s-temp-bed)))))

(define last-reading-time (hash-ref last-reading 'timestamp))

(printf "time since last reading: ~v seconds\n" (- ts last-reading-time))

