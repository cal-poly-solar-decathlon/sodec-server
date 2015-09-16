#lang racket/base

;; I hate to say it, but InfluxDB seems to suffer from poor
;; json encoding design in places. EG: on no results, there's
;; no 'series' list. The empty list is a list, you know....

(require #;racket/runtime-path
         racket/contract
         rackunit
         json
         racket/date
         racket/match
         net/url
         "device-table.rkt"
         "testing-param.rkt")

(provide 
         #;devices-list
         (contract-out
          [struct event ([timestamp exact-integer?]
                         [reading exact-integer?])]
          [device-latest-reading
           (-> measurement? device? (or/c false? exact-integer?))]
          [device-events-in-range
           (-> measurement? device? exact-integer? exact-integer? (listof event?))]
          [count-device-events-in-range
           (-> measurement? device? exact-integer? exact-integer? 64-bit-int?)]
          [record-device-status!
           (->* (measurement? device? 64-bit-int?)
                (#:timestamp exact-integer?) void?)]
          [maybe-reading->jsexpr
           (-> (or/c false? integer?) jsexpr?)]
          [current-timestamp
           (-> exact-integer?)]
          [events->jsexpr (-> (listof event?) jsexpr?)])
         testing?
         reset-database-test-tables!)

(define (false? x) (eq? x #false))

;; is this one of the known measurements?
(define (measurement? m)
  (and (string? m)
       (member m MEASUREMENT-NAMES)))

;; is this a legal device name?
(define LEGAL-DEVICE-REGEXP #px"^[_a-zA-Z0-9]+$")
(define (device? d)
  (and (string? d)
       (regexp-match LEGAL-DEVICE-REGEXP d)))

;; can a number be represented as a 64-bit int?
;(: 64-bit-int? (Integer -> Boolean))
(define (64-bit-int? x)
  (<= MIN64INT x MAX64INT))
(define MAX64INT #x7fffffffffffffff)
(define MIN64INT (- #x8000000000000000))

(struct event (timestamp reading) #:transparent)

;; return the current time
(define (current-timestamp)
  (current-seconds))


(define TESTING-DB "sodec_test")
(define REGULAR-DB "sodec")

;; when testing, use the testing events table
(define (DATABASE)
  (cond [(testing?) TESTING-DB]
        [else REGULAR-DB]))




;; return the latest device reading from one device.
;; choose arbitrarily in case of tie.
(define (device-latest-reading measurement device)
  (define response
    (perform-query (format
                    "SELECT LAST(reading) FROM ~a WHERE device='~a'"
                    measurement device)))
  (match (query-response->series response)
    [(list (? series-hash? series))
     (define series-name (hash-ref series 'name))
     (unless (string=? series-name measurement)
       (error 'device-latest-event
              "inferred constraint failed. Expected measurement name ~e, got ~e"
              measurement series-name))
     (define a-list (single-entry-series->alist series))
     (match (assoc "last" a-list)
       [(list dc reading) reading]
       [other (error 'device-latest-event
                     "inferred constraint failed, no column named 'last'.")])]
    [#f ;; query successful, no results
     #f]
    [other (error 'device-latest-event
                  "inferred constraint failed, expected exactly one series in ~e"
                  other)]))

;; return device statuses in some time range (one device),
;; times in seconds
(define (device-events-in-range measurement device start end)
  (define start-ns (* start (expt 10 9)))
  (define end-ns (* end (expt 10 9)))
  (define response
    (perform-query
     (format EVENTS-IN-RANGE-QUERY measurement start-ns end-ns device)))
  (match (query-response->series response)
    [#f null]
    [(list (? series-hash? series))
     (define column-names (hash-ref series 'columns))
     (define time-index (find-column-index column-names "time"))
     (define reading-index (find-column-index column-names "reading"))
     (for/list ([record (in-list (hash-ref series 'values))])
       (event (influx-timestamp->milliseconds (list-ref record time-index))
              (list-ref record reading-index)))]
    [other (error 'count-device-events-in-range
                  "inferred constraint failed, expected #f or one series in ~e"
                  other)]))

;; find the index of an element in an array
(define (find-column-index los s)
  (let loop ([idx 0] [los los])
    (cond [(null? los) (raise-argument-error
                         'find-column-index
                         (format "list containing ~e" s)
                         0 los s)]
          [(equal? (car los) s) idx]
          [else (loop (add1 idx) (cdr los))])))

(define EVENTS-IN-RANGE-QUERY
  "SELECT * FROM ~a WHERE time > ~a AND time < ~a AND device = '~a'")

;; return device statuses in some time range (one device),
;; times in seconds
(define (count-device-events-in-range measurement device start end)
  (define start-ns (* start (expt 10 9)))
  (define end-ns (* end (expt 10 9)))
  (define response
    (perform-query
     (format COUNT-EVENTS-IN-RANGE-QUERY measurement start-ns end-ns device)))
  (match (query-response->series response)
    [#f 0]
    [(list (? series-hash? series))
     (match (assoc "count" (single-entry-series->alist series))
       [(list "count" (? integer? n)) n]
       [(list "count" 'null) 0]
       [other (error 'count-device-events-in-range
                     "inferred constraint failed, expected count, got ~e"
                     other)])]
    [other (error 'count-device-events-in-range
                  "inferred constraint failed, expected #f or one series in ~e"
                  other)]))

(define COUNT-EVENTS-IN-RANGE-QUERY
  "SELECT COUNT(reading) FROM ~a WHERE time > ~a AND time < ~a AND device = '~a'")


;; given a measurement and a location/device and a reading, write them to
;; the database
(define (record-device-status! measurement device reading
                               #:timestamp [timestamp-ms #f])
  (define timestamp (cond [timestamp-ms timestamp-ms]
                          [else (inexact->exact
                                 (round (current-inexact-milliseconds)))]))
  (define point-line
    (format "~a,device=~a reading=~ai ~a"
            measurement device reading timestamp))
  (define-values (status-line headers port)
    (http-sendrecv/url
     (build-url "write" `((precision . "ms")))
     #:method #"POST"
     #:data (string->bytes/utf-8 point-line)))
  (unless (regexp-match #px"^HTTP/1.1 204" status-line)
    (error 'perform-query
           "expected '204 No Content' status line, got: ~v"
           status-line)))

;;
;; INFLUXDB PARSING
;;

;; is this the influxbd representation of a "series"?
(define (series-hash? s)
  (match s
    [(hash-table ('name dc1) ('values dc2) ('columns dc3)) #t]
    [else #f]))

;; given a single-entry series (a hash table with keys name, value, and columns), return
;; the association list representing the table itself.
(define (single-entry-series->alist series)
  (match series
    [(hash-table ('name name)
                 ('values (list values))
                 ('columns columns))
     (unless (and (list? values) (list? columns)
                  (= (length values) (length columns)))
       (error 'single-entry-series->alist
              "inferred constraint failed for values and columns: ~e, ~e"
              values columns))
     (map list columns values)]
    [other (error 'single-entry-series->alist
                  "inferred constraint failed, expected single-entry series in ~e"
                  other)]))




;; given query results, return the series as a list or #f if no results
(define (query-response->series results)
  (match results
    [(hash-table ('results (list result)))
     (match result
       [(hash-table ('series series-list)) series-list]
       [(hash-table) #f]
       [other (error 'query-response->series
                     "inferred constraint failed, results had unexpected form: ~e"
                     other)])]
    [other (error 'query-response->series
                  "inferred constraint failed, JSON had unexpected form: ~e"
                  other)]))


;; construct a URL to communicate with influxdb
(define (build-url endpoint extra-query-fields)
  (url
   "http"
   #f ;; user
   "localhost"
   8086
   #t ;; absolute
   (list (path/param endpoint '()))
   (cons `(db . ,(DATABASE))
         extra-query-fields)
   #f ;;fragment
   ))

(define (regexp-member rx l)
  (ormap (λ (h) (regexp-match rx h)) l))

;; perform the query, return the result as a jsexpr
(define (perform-query query)
  (define-values (status-line headers port)
    (http-sendrecv/url (build-url "query" `((q . ,query)))))
  (unless (regexp-match #px"^HTTP/1.1 200" status-line)
    (error 'perform-query
           "expected '200 OK' status line, got: ~v with response port content: ~e"
           
           status-line
           (regexp-match #px".*" port)))
  (unless (regexp-member #px#"(?i:Content-Type: +application/json)" headers)
    (error 'perform-query
           "expected content-type application/json, got these headers: ~v"
           headers))
  (read-json port))

;; convert a list of events to jsexprs
(define (events->jsexpr events)
  (map event->jsexpr events))

;; convert a single event to a jsexpr
(define (event->jsexpr event)
  (hash 't (event-timestamp event)
        'r (event-reading event)))


;;
;; TESTING
;;

;; create temporary tables for testing
(define (reset-database-test-tables!)
  ;; ignore a possible error here (db may not exist):
  (perform-query (format "DROP DATABASE ~a" TESTING-DB))
  (match (perform-query (format "CREATE DATABASE ~a" TESTING-DB))
    [(hash-table ('results (list (hash-table)))) (void)]
    [other
     (error 'reset-database-test-tables!
            "unexpected response from table creation: ~e"
            other)]))

;; convert an Event to a jsexpr
;; convert a temperature event to a jsexpr
(define (maybe-reading->jsexpr reading)
  (cond [(integer? reading) reading]
        [else "no events"]))


(define (influx-timestamp->milliseconds str)
  (match (regexp-match TIMESTAMP-REGEXP str)
    [(list dc year month day hour minute second maybe-ms)
     (define ms (cond [maybe-ms
                       (inexact->exact
                        (round (* 1000 (string->number maybe-ms))))]
                      [else 0]))
     (+ ms
        (* 1000
           (find-seconds (string->number second)
                         (string->number minute)
                         (string->number hour)
                         (string->number day)
                         (string->number month)
                         (string->number year))))]))

(define TIMESTAMP-REGEXP
  #px"^([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})T([[:digit:]]{2}):([[:digit:]]{2}):([[:digit:]]{2})(\\.[[:digit:]]+)?Z$")

(module+ test
  (require rackunit)

  (check-equal? (influx-timestamp->milliseconds "2015-09-12T14:25:53.247Z")
                (+
                 (* 1000 (find-seconds 53
                                       25
                                       14
                                       12
                                       9
                                       2015))
                 247))

  (check-equal? (influx-timestamp->milliseconds "2015-09-12T14:25:53.24Z")
                (+
                 (* 1000 (find-seconds 53
                                       25
                                       14
                                       12
                                       9
                                       2015))
                 240))
  
  (check-equal? (influx-timestamp->milliseconds "2015-09-12T14:25:53Z")
                (* 1000 (find-seconds 53
                                      25
                                      14
                                      12
                                      9
                                      2015))))