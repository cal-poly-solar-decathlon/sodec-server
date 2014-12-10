#lang typed/racket/base

(require racket/runtime-path
         typed/rackunit
         typed/racket/date
         racket/match
         "ids.rkt"
         "testing-param.rkt"
         "time.rkt")

(require/typed db/base
               [#:opaque Connection connection?]
               [query-exec (Connection String Any * -> Void)]
               [query-rows (Connection String Any * ->
                                       (Listof (Vectorof Any)))]
               [query-value (Connection String ->
                                        Any)])

(require/typed db/sqlite3
               [sqlite3-connect
                (#:database Path-String
                            [#:mode 'create]-> Connection)])

(provide record-sensor-status!
         sensor-events
         sensor-latest-event
         (struct-out Event)
         maybe-event->jsexpr)

(struct Event ([device-id : ID]
               [timestamp : Natural]
               [status : String])
  #:transparent)

(define-runtime-path here ".")

;; create the database when it doesn't exist:
(when (not (file-exists? (build-path here "sodec.db")))
  (log-warning "creating database")
  (define conn (sqlite3-connect #:database (build-path here "sodec.db")
                                #:mode 'create))
  (query-exec conn "CREATE TABLE events (id TEXT, timestamp INTEGER, value TEXT)"))

;; create a connection to the database
(define conn (sqlite3-connect #:database (build-path here "sodec.db")))


;; when testing, use the testing events table
(define (event-table)
  (cond [(testing?) "test_events"]
        [else "events"]))


;; NB: sqlite doesn't do ... well, nearly every kind of type.
;; I'm just using "text" to represent dates, because that's what we've got...
;; EXTREMELY DANGEROUS. DESTROYS ALL DATA
(define (reset-database!)
  (query-exec conn "DROP TABLE events")
  (query-exec conn "CREATE TABLE events (id TEXT, timestamp INTEGER, value TEXT)"))

;; create temporary tables for testing
(define (create-database-test-tables!)
  (query-exec conn "CREATE TEMPORARY TABLE test_events (id TEXT, timestamp INTEGER, value TEXT)"))

;; safe to do this every time...
(create-database-test-tables!)

;; type stuff: convert a vector of values to an Event
(: row->event ((Vectorof Any) -> Event))
(define (row->event row)
  (match row
    [(vector (? ID? id)
             (? exact-nonnegative-integer? timestamp)
             (? string? status))
     (Event id timestamp status)]
    [other
     (raise-argument-error 'row->event
                           "vector containing string, nat, string"
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
(: record-sensor-status! (String String -> Void))
(define (record-sensor-status! id status)
  (define timestamp (current-timestamp))
  (query-exec conn
              (string-append "INSERT INTO " (event-table) " VALUES (?,?,?)")
              id timestamp status))

;; return all sensor statuses (from all times, one sensor)
(: sensor-events (String -> (Listof Event)))
(define (sensor-events id)
  (map row->event 
       (query-rows 
        conn 
        (string-append "SELECT * FROM "(event-table)" WHERE ID=? ORDER BY timestamp")
        id)))

;; return the latest sensor status from one sensor.
;; choose arbitrarily in case of tie.
(: sensor-latest-event (String -> (U False Event)))
(define (sensor-latest-event id)
  ;; there *must* be a better way to do this:
  (define db-hits
    (query-rows 
     conn 
     (string-append "SELECT * FROM "(event-table)" WHERE ID=? AND TIMESTAMP = "
                    "(SELECT MAX(timestamp) FROM "(event-table)" WHERE ID=?)")
     id id))
  (match db-hits
    [(list) #f]
    [(list only-hit) (row->event only-hit)]
    [(cons first-hit others) (row->event first-hit)]))

;; convert an Event to a jsexpr
;; convert a temperature event to a jsexpr
(: maybe-event->jsexpr ((U Event False) -> (U (HashTable Symbol Any) String)))
(define (maybe-event->jsexpr event)
  (cond [(Event? event)
         (make-immutable-hash
          (list (cons 'device-id (Event-device-id event))
                (cons 'timestamp (Event-timestamp event))
                (cons 'status (Event-status event))))]
        [else "no events"]))

(parameterize ([testing? #t])
  
  
  (define ts1 EPOCH)
  
  (record-sensor-status! "s-temp-lr" "32279")
  
  ;; this is the latest:
  (check-equal? (sensor-latest-event "s-temp-lr")
                (Event "s-temp-lr" EPOCH "32279"))

  (record-sensor-status! "s-temp-kit" "22900")
  
  ;; this is still the latest:
  (check-equal? (sensor-latest-event "s-temp-lr")
                (Event "s-temp-lr" EPOCH "32279"))
  
  (increment-test-timestamp!)
  (record-sensor-status! "s-temp-lr" "33116")
  
  ;; now the latest has changed:
  (check-equal? (sensor-latest-event "s-temp-lr")
                (Event "s-temp-lr" (+ 1 EPOCH) "33116"))

  (increment-test-timestamp!)
  (record-sensor-status! "s-temp-kit" "22883")
 
  
  (check-equal? (maybe-event->jsexpr (sensor-latest-event "s-temp-lr"))
                (make-immutable-hash 
                 (list (cons 'timestamp (+ 1 EPOCH))
                       (cons 'device-id "s-temp-lr")
                       (cons 'status "33116"))))
  
  (check-equal?
   (sensor-events "s-temp-kit")
   (list (Event "s-temp-kit" EPOCH "22900")
         (Event "s-temp-kit" (+ 2 EPOCH) "22883")))
  
  )
