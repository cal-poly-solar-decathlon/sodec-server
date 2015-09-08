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
         "influx-ids.rkt"
         "testing-param.rkt")

(provide 
         #;devices-list
         #;sensor-events
         
         #;sensor-events-in-range
         (contract-out
          [sensor-latest-reading
           (-> string? string? (or/c false? exact-integer?))]
          [count-sensor-events-in-range
           (-> string? string? integer? integer? exact-integer?)]
          [record-sensor-status!
           (-> string? string? integer? void?)]
          [maybe-reading->jsexpr
           (-> (or/c false? integer?) jsexpr?)]
          [current-timestamp
           (-> exact-integer?)])
         #;events->jsexpr
         #;events->jsexpr/short
         testing?
         reset-database-test-tables!)

(define (false? x) (eq? x #false))



;; return the current time
(define (current-timestamp)
  (current-seconds))


(define TESTING-DB "sodec_test")
(define REGULAR-DB "sodec")

;; when testing, use the testing events table
(define (DATABASE)
  (cond [(testing?) TESTING-DB]
        [else REGULAR-DB]))




;; return the latest sensor reading from one sensor.
;; choose arbitrarily in case of tie.
(define (sensor-latest-reading measurement device)
  (unless (member measurement MEASUREMENT-NAMES)
    (raise-argument-error 'perform-write "legal measurement name"
                          0 measurement device))
  (define devices (cadr (assoc measurement measurement-devices)))
  (unless (member device devices)
    (raise-argument-error 'perform-write "legal location for measurement"
                          1 measurement device))
  (define response
    (perform-query (format
                    "SELECT LAST(reading) FROM ~a WHERE device='~a'"
                    measurement device)))
  (match (query-response->series response)
    [(list (? series-hash? series))
     (define series-name (hash-ref series 'name))
     (unless (string=? series-name measurement)
       (error 'sensor-latest-event
              "inferred constraint failed. Expected measurement name ~e, got ~e"
              measurement series-name))
     (define a-list (single-entry-series->alist series))
     (match (assoc "last" a-list)
       [(list dc reading) reading]
       [other (error 'sensor-latest-event
                     "inferred constraint failed, no column named 'last'.")])]
    [#f ;; query successful, no results
     #f]
    [other (error 'sensor-latest-event
                  "inferref constraint failed, expected exactly one series in ~e"
                  other)]))

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

(define EVENTS-IN-RANGE-QUERY
  "SELECT COUNT(reading) FROM ~a WHERE time > ~a AND time < ~a AND device = '~a'")

;; return sensor statuses in some time range (one sensor),
;; times in seconds
#;(: count-sensor-events-in-range (String date date -> Natural))
(define (count-sensor-events-in-range measurement device start end)
  (define start-ns (* start (expt 10 9)))
  (define end-ns (* end (expt 10 9)))
  (define response
    (perform-query
     (format EVENTS-IN-RANGE-QUERY measurement start-ns end-ns device)))
  (match (query-response->series response)
    [#f 0]
    [(list (? series-hash? series))
     (match (assoc "count" (single-entry-series->alist series))
       [(list "count" (? integer? n)) n]
       [other (error 'count-sensor-events-in-range
                     "inferred constraint failed, expected count, got ~e"
                     other)])]
    [other (error 'count-sensor-events-in-range
                  "inferred constraint failed, expected #f or one series in ~e"
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


;; find the latest event of a sensor
;; THIS IS HORRIBLE...InfluxDB REALLY should be doing this
;; for us. Perform a binary search to find the ...
#;(define )

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

(define MEASUREMENT-NAMES (map car measurement-devices))

;; given a measurement and a location/device and a reading, write them to
;; the database
(define (record-sensor-status! measurement device reading)
  (unless (member measurement MEASUREMENT-NAMES)
    (raise-argument-error 'perform-write "legal measurement name"
                          0 measurement device reading))
  (define devices (cadr (assoc measurement measurement-devices)))
  (unless (member device devices)
    (raise-argument-error 'perform-write "legal location for measurement"
                          1 measurement device reading))
  (unless (64-bit-int? reading)
    (raise-argument-error 'perform-write "64-bit integer"
                          2 measurement device reading))
  (define point-line
    (format "~a,device=~a reading=~ai ~a"
            measurement device reading
            (inexact->exact (round (current-inexact-milliseconds)))))
  (define-values (status-line headers port)
    (http-sendrecv/url
     (build-url "write" `((precision . "ms")))
     #:method #"POST"
     #:data (string->bytes/utf-8 point-line)))
  (unless (regexp-match #px"^HTTP/1.1 204" status-line)
    (error 'perform-query
           "expected '204 No Content' status line, got: ~v"
           status-line)))


;; can a number be represented as a 64-bit int?
;(: 64-bit-int? (Integer -> Boolean))
(define (64-bit-int? x)
  (<= MIN64INT x MAX64INT))
(define MAX64INT #x7fffffffffffffff)
(define MIN64INT (- #x8000000000000000))

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

;; record a sensor reading
;(: record-sensor-status! (String Integer -> Void))
#;(define (record-sensor-status! id status)
  (unless (64-bit-int? status)
    (raise-argument-error 'record-sensor-status!
                          "integer representable in 64 bits"
                          1 id status))
  (query-exec conn
              (string-append "INSERT INTO " (event-table) " VALUES (DEFAULT,?,DEFAULT,?)")
              id status))


;;;;;;
;;
;; JSON
;;
;;;;;;

;; convert an Event to a jsexpr
;; convert a temperature event to a jsexpr
(define (maybe-reading->jsexpr reading)
  (cond [(integer? reading) reading]
        [else "no events"]))

#;(

(: ensure-nat (Any -> Natural))
(define (ensure-nat val)
  (cond [(exact-nonnegative-integer? val) val]
        [else (raise-argument-error 
               'ensure-nat
               "exact nonnegative integer"
               0 val)]))


;; return all sensor statuses (from all times, one sensor)
(: sensor-events (String -> (Listof SensorEvent)))
(define (sensor-events id)
  (map row->event 
       (query-rows 
        conn 
        (string-append "SELECT * FROM "(event-table)" WHERE device=? ORDER BY id")
        id)))

;; return sensor statuses in some time range (one sensor)
(: sensor-events-in-range (String date date -> (Listof SensorEvent)))
(define (sensor-events-in-range id start end)
  (map row->event
       (query-rows 
        conn
        (string-append
         "SELECT * FROM "(event-table)" WHERE device=? "
         "AND timestamp >= ? "
         "AND timestamp < ? "
         "ORDER BY id")
        id
        (date->timestamp start)
        (date->timestamp end))))

;; return sensor statuses in some time range (one sensor)
(: count-sensor-events-in-range (String date date -> Natural))
(define (count-sensor-events-in-range id start end)
  (ensure-nat
   (query-value
    conn
    (string-append
     "SELECT COUNT(*) FROM "(event-table)" WHERE device=? "
     "AND timestamp >= ? "
     "AND timestamp < ? ")
    id
    (date->timestamp start)
    (date->timestamp end))))

;; convert a list of events to a jsexpr
;; convert a temperature event to a jsexpr
(: events->jsexpr ((Listof SensorEvent) -> (Listof (HashTable Symbol Any))))
(define (events->jsexpr events)
  (map event->jsexpr events))

;; convert a list of events to a jsexpr
;; convert a temperature event to a jsexpr
(: events->jsexpr/short ((Listof SensorEvent) -> (U String (HashTable Symbol Any))))
(define (events->jsexpr/short events)
  (cond [(null? events) "no events"]
        [else
         (define diffs
           (for/list : (Listof (List Integer Integer))
             ([event-a (in-list events)]
              [event-b (in-list (cdr events))])
             (list (- (date->seconds (SensorEvent-timestamp event-b))
                      (date->seconds (SensorEvent-timestamp event-a)))
                   (- (SensorEvent-reading event-b)
                      (SensorEvent-reading event-a)))))
         (make-immutable-hash
          (ann
           (list (cons 'baseTimestamp (date->seconds (SensorEvent-timestamp (car events))))
                 (cons 'baseStatus (SensorEvent-reading (car events)))
                 (cons 'seriesData diffs))
           (Listof (Pairof Symbol Any))))]))

;; convert a row to a device jsexpr
(: device-row->jsexpr ((Vectorof Any) -> (HashTable Symbol String)))
(define (device-row->jsexpr row)
  (match row
    [(vector (? string? device-name)
             (? string? device-description))
     (make-immutable-hash
      (list (cons 'device device-name)
            (cons 'description device-description)))]
    [other
     (raise-argument-error 'row->event
                           "vector containing string, string"
                           0
                           row)]))
;;;;;
;;
;; TIME
;;
;;;;;


;; convert a mysql timestamp string to a racket date
(: timestamp->date (sql-timestamp -> date))
(define (timestamp->date timestamp)
  (unless (eq? #f (sql-timestamp-tz timestamp))
    (raise-argument-error 'timestamp->date "sql timestamp with tz=false"
                          0 timestamp))
  (seconds->date
   (find-seconds (sql-timestamp-second timestamp)
                 (sql-timestamp-minute timestamp)
                 (sql-timestamp-hour timestamp)
                 (sql-timestamp-day timestamp)
                 (sql-timestamp-month timestamp)
                 (sql-timestamp-year timestamp))))

(: date->timestamp (date -> sql-timestamp))
(define (date->timestamp date)
  (sql-timestamp (date-year date)
                 (date-month date)
                 (date-day date)
                 (date-hour date)
                 (date-minute date)
                 (date-second date)
                 0
                 #f)))
