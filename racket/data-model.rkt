#lang typed/racket/base

(require racket/runtime-path
         typed/rackunit
         typed/racket/date
         racket/match
         "ids.rkt"
         "testing-param.rkt")

(require/typed db/base
               [#:opaque Connection connection?]
               [#:opaque ConnectionPool connection-pool?]
               [virtual-connection (ConnectionPool -> Connection)]
               [connection-pool ((-> Connection) -> ConnectionPool)]
               [query-exec (Connection String Any * -> Void)]
               [query-rows (Connection String Any * ->
                                       (Listof (Vectorof Any)))]
               [query-value (Connection String Any * ->
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
                (#:user String #:password String #:database String
                        (#:socket Path-String) -> Connection)])

(require/typed "mysql-socket.rkt"
               [mysql-socket String])


(provide current-timestamp
         devices-list
         sensor-events
         sensor-latest-event
         sensor-events-in-range
         count-sensor-events-in-range
         record-sensor-status!
         (struct-out SensorEvent)
         maybe-event->jsexpr
         events->jsexpr
         events->jsexpr/short
         testing?
         reset-database-test-tables!)

(struct SensorEvent ([device : ID]
                     [timestamp : date]
                     [reading : Integer])
  #:transparent)

(define-runtime-path here ".")

;; create a connection to the database
(define conn 
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:database "sodec2"
                     #:user "clements"
                     #:password "aoeuidht"
                     #:socket mysql-socket)))))


;; when testing, use the testing events table
(define (event-table)
  (cond [(testing?) "test_sensorevents"]
        [else "sensorevents"]))


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
  INDEX (`timestamp`) USING BTREE,
  CONSTRAINT `temp_sensorevents_ibfk_11234` FOREIGN KEY (`device`) REFERENCES `devices` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
|
  )

;; create temporary tables for testing
(define (reset-database-test-tables!)
  (with-handlers ([exn:fail?
                  (lambda (exn)
                    (fprintf (current-error-port)
                             "drop table failed. Proceeding...\n"))])
    (query-exec conn "DROP TABLE test_sensorevents;"))
  (query-exec conn test-table-sql-stmt))


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
        (string-append "SELECT * FROM "(event-table)" WHERE device=? ORDER BY timestamp")
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
         "ORDER BY timestamp")
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

;; return the latest sensor status from one sensor.
;; choose arbitrarily in case of tie.
(: sensor-latest-event (String -> (U False SensorEvent)))
(define (sensor-latest-event id)
  (unless (ID? id)
    (raise-argument-error 'sensor-latest-event
                          "known device ID"
                          0 id))
  (define db-hits
    (query-rows 
     conn 
     (string-append
      "SELECT * FROM "(event-table)" WHERE DEVICE=? " 
      "ORDER BY timestamp DESC " 
      "LIMIT 1;")
     id))
  (match db-hits
    [(list) #f]
    [(list only-hit) (row->event only-hit)]
    [(cons first-hit others)
     (error 'sensor-latest-event 
            "internal error: limit 1 query returned >1 result")]))


;; return the list of devices
(: devices-list (-> (Listof (HashTable Symbol String))))
(define (devices-list)
  (define db-hits
    (query-rows
     conn 
     (string-append
      "SELECT name,description FROM devices;")))
  (map device-row->jsexpr db-hits))

;;;;;;
;;
;; JSON
;;
;;;;;;

;; convert an Event to a jsexpr
;; convert a temperature event to a jsexpr
(: maybe-event->jsexpr ((U SensorEvent False) -> (U (HashTable Symbol Any) String)))
(define (maybe-event->jsexpr event)
  (cond [(SensorEvent? event)
         (event->jsexpr event)]
        [else "no events"]))

;; convert a list of events to a jsexpr
;; convert a temperature event to a jsexpr
(: events->jsexpr ((Listof SensorEvent) -> (Listof (HashTable Symbol Any))))
(define (events->jsexpr events)
  (map event->jsexpr events))

;; convert an Event to a jsexpr
(: event->jsexpr (SensorEvent -> (HashTable Symbol Any)))
(define (event->jsexpr event)
  (make-immutable-hash
   (list (cons 'device-id (SensorEvent-device event))
         (cons 'timestamp (date->seconds (SensorEvent-timestamp event)))
         (cons 'status (SensorEvent-reading event)))))

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

;; get the current timestamp from the mysql server
(: current-timestamp (-> date))
(define (current-timestamp)
  (define server-timestamp 
    (query-value conn "SELECT CURRENT_TIMESTAMP;"))
  (cond [(sql-timestamp? server-timestamp) (timestamp->date server-timestamp)]
        [else (error 'get-timestamp
                     "expected string from server, got: ~v\n" server-timestamp)]))

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
                 #f))
