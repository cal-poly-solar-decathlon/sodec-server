#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json)

;; given a URL, make a GET request and wait for a response, returning a jsexpr
(define (remote-call/get url-string)
  (results->jsexpr (remote-call/get/core url-string)))

;; given a URL, make a POST request and wait for a succesful response, returning a jsexpr
(define (remote-call/post url-string post-bytes)
  (results->jsexpr (remote-call/post/core url-string post-bytes)))

;; given a list of results, ensure that the return code is 200 and then parse
;; the body as a jsexpr
(define (results->jsexpr results)
  (match-define (list response-code first-line headers body-port) results)
  (cond [(= response-code 200)
         (define mime-type (extract-field "Content-Type" headers))
         (unless (regexp-match #px"^application/json(;.*)?$" mime-type)
           (error 'remote-call/get
                  (format "expected mime type application/json, got ~e"
                          mime-type)))
         (define reply (first (regexp-match #px".*" body-port)))
         (close-input-port body-port)
         (log-debug  (format "reply-bytes : ~v\n" reply))
         (bytes->jsexpr reply)]
        [else
         (fail-check
          (format
           "response code: expected 200, got: ~v\nwith message: ~v\nand body: ~v" 
           response-code
           first-line
           (regexp-match #px".*" body-port)))]))

;; given a URL string, return the response code, the first line, the rest
;; of the headers, and the port for the remainder of the body
(define (remote-call/get/core url-string)
  (log-debug "remote-call/get/core: url-string ~a"
             url-string)
  (response-port->results (get-impure-port (string->url url-string))))

;; given a URL string and a POST body, make a POST request, return the response
;; code, the first line, the rest of the headers, and the port for the remainder of the body.
(define (remote-call/post/core url-string post-bytes)
  (log-debug "remote-call/post/core: url-string ~a, post-bytes: ~a"
             url-string post-bytes)
  (response-port->results (post-impure-port (string->url url-string)
                                            post-bytes
                                            (list
                                             "Content-Type: application/json"))))

;; given an input port, return the response code, the first line, the rest of the headers,
;; and the port for the body
(define (response-port->results response-port)
  ;; what about timeouts?
  (define header-string (purify-port response-port))
  ;; strange... why do our servers always come back with text/html as 
  ;; a mime type?
  (match (regexp-match #px"^([^\n]*)\n(.*)" header-string)
    [(list dc first-line headers)
     (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" first-line)
       [(list dc2 response-code-string)
        (define reply-code (string->number response-code-string))
        (unless (regexp-match #px"Access-Control-Allow-Origin: \\*"
                              headers)
          (error 'response-port->results
                 "didn't find expected CORS header in headers: ~e\n"
                 headers))
        (list reply-code first-line headers response-port)]
       [other
        (error 'remote-call/get/core
               "couldn't extract response code from first response line ~e"
               first-line)])]
    [other (error 'remote-call/get
                  (format "expected response with at least one header line, got ~e"
                          header-string))]))


(define l-u 
  ;; test locally:
  "http://localhost:8080"
  ;; test brinckerhoff.org (whatever it points to)
  #;"http://calpolysolardecathlon.org:8080"
  #;"http://calpolysolardecathlon.org:3000")

(define (rel-url str)
  (string-append l-u str))


(define (call-subpath subpath)
  (remote-call/get (rel-url (~a "/srv" subpath))))

(define (call-subpath/post subpath post-bytes)
  (remote-call/post (rel-url (~a "/srv" subpath)) post-bytes))

;; events in last hour on the "s-temp-bed" device
(define (events-in-last-hour)
  (define ts (hash-ref (call-subpath "/timestamp") 'timestamp))
  ;; this is getting a bit nasty in the string-append region...
  (call-subpath (let ([ans (~a "/events-in-range?device=s-temp-bed;start="
                               (- ts 3600)
                               ";end="
                               ts)])
                  (printf "~s\n" ans)
                  ans)))

;; ping check returns wrong result
;; no events wrong in both places

(run-tests
(test-suite
 "racket evaluator tests"
 (let ()


   (test-equal? "ping" (call-subpath "/ping") "alive")

   (test-case
    "timestamp"
    (match (call-subpath "/timestamp")
      [(hash-table ('timestamp (? number? n)))
       (check > n (find-seconds 0 0 0 1 1 2014))
       (check < n (find-seconds 0 0 0 1 1 2016))]
      [other 
       (fail "timestamp shape")]))
   
   (define ((port-containing str) port)
     (regexp-match (regexp-quote str) port))

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
    
    ;; near miss on the device name:
    (check-match (remote-call/get/core (rel-url "/srv/latest-event?device=uhnoth"))
                 (list (or 404 400)
                       (regexp #px"^HTTP/1.1 40[04]")
                       _2
                       _3
                       ;; sigh...
                       #;(? (port-containing "uhnoth") _3))))
   
   
   (test-equal? "empty-latest-events"
                (call-subpath "/latest-event?device=s-temp-testing-empty")
                "no events")

   (test-case
    "latest-living-room-event"
   (check-match
    (call-subpath "/latest-event?device=s-temp-lr")
    (hash-table ('timestamp (? number? n))
                  ('device-id "s-temp-lr")
                  ('status (? number? s)))))
   

   (test-case
    "events-in-empty-range"
    (check-equal?
     (call-subpath "/events-in-range?device=s-temp-bed;start=0;end=0")
     "no events"))
   
   (test-case
    "events-in-range"
    (check-match (events-in-last-hour)
                 (hash-table ('baseTimestamp (? number? _1))
                             ('baseStatus (? values _2))
                             ('seriesData (? values _3))))
    
    ;; more than a day of data:
    (check-match (remote-call/get/core 
                  (rel-url
                   "/srv/events-in-range?device=s-temp-bed;start=0;end=100000"))
                 (list 400
                       "HTTP/1.1 400 range too long\r"
                       _2
                       _3)))

   (test-case
    "count-events-in-range"
    (check-match (remote-call/get/core
                  (rel-url "/srv/count-events-in-range"))
                 (list 404
                       "HTTP/1.1 404 wrong query fields\r" _2 _3))

    (check-match (remote-call/get/core
                  (rel-url "/srv/count-events-in-range?device=foo;start=0;end=1"))
                 (list 404
                       "HTTP/1.1 404 wrong query fields\r"  _2 _3))

    (define ((number-in-range a b) n)
      (and (<= a n) (< n b)))
    
    (check-pred (number-in-range 10 722)
                (let ([ts (hash-ref (call-subpath "/timestamp") 'timestamp)])
                (remote-call/get 
                 (rel-url (~a
                           "/srv/count-events-in-range?device=s-temp-lr;start="
                           (- ts 3600)";end="ts))))))

   ;; RECORDING READINGS

   (test-case
    "record-reading-404"
    (check-match (remote-call/post/core
                  (string-append l-u "/srv/record-reading?device=uhnoth")
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
                  (string-append l-u "/srv/record-reading?device=s-temp-lr")
                  #"abcd")
                 (list 400
                       "HTTP/1.1 400 bad JSON in POST\r"
                       _2
                       _3)))
   
   (test-case
    "record-reading"
    (check-equal? (remote-call/post
                   (string-append l-u "/srv/record-reading?device=s-temp-testing-blackhole")
                   #"{\"status\":7772387,\"secret\":\"$a8Es#crB469\"}")
                  "okay"))
)))


(define ts (hash-ref (call-subpath "/timestamp") 'timestamp))

;; this is getting a bit nasty in the string-append region...
(define last-hour-jsexpr
  (call-subpath (~a "/events-in-range?device=s-temp-bed;start="
                    (- ts 3600)
                    ";end="
                    ts)))

(printf "number of readings in the last hour: ~v\n"
        (add1 (length (hash-ref last-hour-jsexpr 'seriesData))))

(define jsexpr-len
  (bytes-length
   (jsexpr->bytes last-hour-jsexpr)))

(printf "bytes per reading: ~v\n" (/ jsexpr-len 
                                     (add1 (length 
                                            (hash-ref last-hour-jsexpr
                                                      'seriesData)))))

(define last-reading
  (call-subpath "/latest-event?device=s-temp-bed"))

(define last-reading-time (hash-ref last-reading 'timestamp))

(printf "time since last reading: ~v seconds\n" (- ts last-reading-time))

