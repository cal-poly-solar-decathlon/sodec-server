#lang typed/racket/base

(require racket/runtime-path
         typed/rackunit
         typed/racket/date
         racket/match
         "ids.rkt"
         "testing-param.rkt")

(require/typed db/base
               [#:opaque Connection connection?]
               [query-exec (Connection String Any * -> Void)]
               [query-rows (Connection String Any * ->
                                       (Listof (Vectorof Any)))]
               [query-value (Connection String ->
                                        Any)]
               [#:struct sql-timestamp ([year : Natural]
                                        [month : Natural]
                                        [day : Natural]
                                        [hour : Natural]
                                        [minute : Natural]
                                        [second : Natural]
                                        [nanosecond : Natural]
                                        [tz : (U Natural False)])])

(require/typed db/mysql
               [mysql-connect
                (#:user String #:password String #:database String -> Connection)])


(provide record-sensor-status!
         sensor-events
         sensor-latest-event
         (struct-out SensorEvent)
         maybe-event->jsexpr
         current-timestamp)

(struct SensorEvent ([device : ID]
                     [timestamp : date]
                     [reading : Integer])
  #:transparent)

(define-runtime-path here ".")

;; create a connection to the database
(define conn (mysql-connect #:database "sodec2"
                            #:user "clements"
                            #:password "aoeuidht"))


;; when testing, use the testing events table
(define (event-table)
  (cond [(testing?) "test_sensorevents"]
        [else "sensorevents"]))


;; NB: sqlite doesn't do ... well, nearly every kind of type.
;; I'm just using "text" to represent dates, because that's what we've got...
;; EXTREMELY DANGEROUS. DESTROYS ALL DATA
#;(define (reset-database!)
  (query-exec conn "DROP TABLE events")
  (query-exec conn "CREATE TABLE events (id TEXT, timestamp INTEGER, value TEXT)"))

;; NB: this table can't be temporary--if it is, we can't refer to it twice
;; in the same query.
(define test-table-sql-stmt
  #<<|
CREATE TABLE `test_sensorevents` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `device` varchar(64) NOT NULL,
  `timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `reading` bigint(20) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `device` (`device`),
  CONSTRAINT `temp_sensorevents_ibfk_11234` FOREIGN KEY (`device`) REFERENCES `devices` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
|
  )

;; create temporary tables for testing
(define (create-database-test-tables!)
  (with-handlers ([exn:fail?
                  (lambda (exn)
                    (fprintf (current-error-port)
                             "drop table failed. Proceeding...\n"))])
    (query-exec conn "DROP TABLE test_sensorevents;"))
  (query-exec conn test-table-sql-stmt))

;; safe to do this every time...
(create-database-test-tables!)

;; type stuff: convert a vector of values to an SensorEvent
(: row->event ((Vectorof Any) -> SensorEvent))
(define (row->event row)
  (match row
    [(vector (? exact-nonnegative-integer? event-id)
             (? ID? id)
             (? sql-timestamp? timestamp)
             (? exact-integer? reading))
     (SensorEvent id (timestamp->date timestamp) reading)]
    [other
     (raise-argument-error 'row->event
                           "vector containing nat, string, string, integer"
                           0
                           row)]))

(: ensure-nat (Any -> Natural))
(define (ensure-nat val)
  (cond [(exact-nonnegative-integer? val) val]
        [else (raise-argument-error 
               'ensure-nat
               "exact nonnegative integer"
               0 val)]))

;; record a sensor reading
(: record-sensor-status! (String Integer -> Void))
(define (record-sensor-status! id status)
  (unless (64-bit-int? status)
    (raise-argument-error 'record-sensor-status!
                          "integer representable in 64 bits"
                          1 id status))
  (query-exec conn
              (string-append "INSERT INTO " (event-table) " VALUES (DEFAULT,?,DEFAULT,?)")
              id status))

;; can a number be represented as a 64-bit int?
(: 64-bit-int? (Integer -> Boolean))
(define (64-bit-int? x)
  (<= MIN64INT x MAX64INT))
(define MAX64INT #x7fffffffffffffff)
(define MIN64INT (- #x8000000000000000))

;; return all sensor statuses (from all times, one sensor)
(: sensor-events (String -> (Listof SensorEvent)))
(define (sensor-events id)
  (map row->event 
       (query-rows 
        conn 
        (string-append "SELECT * FROM "(event-table)" WHERE DEVICE=? ORDER BY timestamp")
        id)))

;; return the latest sensor status from one sensor.
;; choose arbitrarily in case of tie.
(: sensor-latest-event (String -> (U False SensorEvent)))
(define (sensor-latest-event id)
  (unless (ID? id)
    (raise-argument-error 'sensor-latest-event
                          "known device ID"
                          0 id))
  ;; this looks nasty but mysql does well with it:
  (define db-hits
    (query-rows 
     conn 
     (string-append
      "SELECT * FROM "(event-table)" WHERE DEVICE=? ORDER BY timestamp "
      "DESC LIMIT 1;")
     id))
  (match db-hits
    [(list) #f]
    [(list only-hit) (row->event only-hit)]
    [(cons first-hit others)
     (error 'sensor-latest-event 
            "internal error: limit 1 query returned >1 result")]))

;; convert an Event to a jsexpr
;; convert a temperature event to a jsexpr
(: maybe-event->jsexpr ((U SensorEvent False) -> (U (HashTable Symbol Any) String)))
(define (maybe-event->jsexpr event)
  (cond [(SensorEvent? event)
         (make-immutable-hash
          (list (cons 'device-id (SensorEvent-device event))
                (cons 'timestamp (date->seconds (SensorEvent-timestamp event)))
                (cons 'status (SensorEvent-reading event))))]
        [else "no events"]))

;;;;;
;;
;; TIME
;;
;;;;;

;; get the current timestamp from the mysql server
(: current-timestamp (-> date))
(define (current-timestamp)
  (cond 
    ;; when testing, use bogus timestamps starting at EPOCH,
    ;; and artificially incremented by calls to increment-test-timestamp!
    [(testing?) (seconds->date (unbox testing-timestamp-box))]
    [else
     (define server-timestamp 
       (query-value conn "SELECT CURRENT_TIMESTAMP;"))
     (cond [(sql-timestamp? server-timestamp) (timestamp->date server-timestamp)]
           [else (error 'get-timestamp
                        "expected string from server, got: ~v\n" server-timestamp)])]))

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



;; the beginning of time for testing
(: EPOCH Natural)
(define EPOCH 
  (let ([epoch-maybe-negative (find-seconds 0 0 0 1 1 1971)])
    (cond [(< epoch-maybe-negative 0) (error 'epoch "epoch was negative!")]
          [else epoch-maybe-negative])))

(: testing-timestamp-box (Boxof Integer))
(define testing-timestamp-box (box EPOCH))



(parameterize ([testing? #t])
  
  (: se->dr ((U False SensorEvent) -> (List ID Integer)))
  (define (se->dr e)
    (cond [(eq? #f e) (error 'se->dr "test fail")]
          [else (list (SensorEvent-device e)
                      (SensorEvent-reading e))]))
  
  (check-not-exn
   (lambda () (record-sensor-status! "s-temp-lr" 32279)))
  
  ;; this is the latest:
  (check-equal? (se->dr (sensor-latest-event "s-temp-lr"))
                (list "s-temp-lr" 32279))

  (record-sensor-status! "s-temp-bed" 22900)
  
  ;; this is still the latest:
  (check-equal? (se->dr (sensor-latest-event "s-temp-lr"))
                (list "s-temp-lr" 32279))
  
  (sleep 1.5)
  (record-sensor-status! "s-temp-lr" 33116)
  
  ;; now the latest has changed:
  (check-equal? (se->dr (sensor-latest-event "s-temp-lr"))
                (list "s-temp-lr" 33116))

  (sleep 1.5)
  (record-sensor-status! "s-temp-bed" 22883)
 
  ;; don't have test times or check-match, :(
  (define hash (maybe-event->jsexpr (sensor-latest-event "s-temp-lr")))
  (cond [(string? hash) (check-equal? "true" "false")]
        [else 
         (check-equal? (hash-ref hash 'device-id) "s-temp-lr")
         (check-true (integer? (hash-ref hash 'timestamp)))
         (check-equal? (hash-ref hash 'status) 33116)])
  
  
  (check-equal?
   (map se->dr (sensor-events "s-temp-bed"))
   (list (list "s-temp-bed" 22900)
         (list "s-temp-bed" 22883)))
  
  (check-not-exn (lambda () (current-timestamp)))
  )

