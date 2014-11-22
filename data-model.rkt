#lang typed/racket/base

(require racket/runtime-path
         typed/rackunit
         typed/racket/date
         racket/match
         
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
         sensor-statuses
         (struct-out Status))

(struct Status ([id : Natural]
                [timestamp : date]
                [status : String])
  #:transparent)

(define-runtime-path here ".")

(define conn (sqlite3-connect #:database (build-path here "sodec.db")))

(define (create-db)
  (sqlite3-connect #:database (build-path here "sodec.db")
                   #:mode 'create))



;; when testing, use the testing status table
(define (status-table)
  (cond [(testing?) "test_status"]
        [else "status"]))


;; NB: sqlite doesn't do ... well, nearly every kind of type.
;; I'm just using "text" to represent dates, because that's what we've got...
;; EXTREMELY DANGEROUS. DESTROYS ALL DATA
(define (reset-database!)
  (query-exec conn "DROP TABLE devices")
  (query-exec conn "DROP TABLE status")
  (query-exec conn "CREATE TABLE status (id INTEGER, timestamp INTEGER, value TEXT)"))

;; create temporary tables for testing
(define (create-database-test-tables!)
  (query-exec conn "CREATE TEMPORARY TABLE test_status (id INTEGER, timestamp INTEGER, value TEXT)"))

;; safe to do this every time...
(create-database-test-tables!)

;; type stuff: convert a vector of values to a Status
(: row->status ((Vectorof Any) -> Status))
(define (row->status row)
  (match row
    [(vector (? exact-nonnegative-integer? id)
             (? exact-nonnegative-integer? timestamp)
             (? string? status))
     (Status id (seconds->date timestamp) status)]
    [other
     (raise-argument-error 'row->status
                           "vector containing nat, nat, string"
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
(: record-sensor-status! (Natural String -> Void))
(define (record-sensor-status! id status)
  (define timestamp (current-timestamp))
  (query-exec conn
              (string-append "INSERT INTO " (status-table) " VALUES (?,?,?)")
              id timestamp status))

;; return all sensor statuses (from all times, all sensors)
(: all-statuses (-> (Listof Status)))
(define (all-statuses)
  (map row->status
       (query-rows conn 
                   (string-append "SELECT * FROM "(status-table)" ORDER BY timestamp"))))

;; return all sensor statuses (from all times, one sensor)
(: sensor-statuses (Natural -> (Listof Status)))
(define (sensor-statuses id)
  (map row->status 
       (query-rows conn 
                   (string-append "SELECT * FROM "(status-table)" WHERE ID=? ORDER BY timestamp")
                   id)))


(parameterize ([testing? #t])
  
  (check-equal?
   (all-statuses)
   (list))
  
  (define ts1 EPOCH)
  (define ts1-pos (cond [(< ts1 0) (error 'ts1-pos "ts1 not positive!")]
                        [else ts1]))
  
  (record-sensor-status! 2 (number->string 32279))
  (record-sensor-status! 1 (number->string 22900))
  (increment-test-timestamp!)
  (record-sensor-status! 2 (number->string 33116))
  (increment-test-timestamp!)
  (record-sensor-status! 1 (number->string 22883))
  
  (check-equal?
   (all-statuses)
   (list (Status 2 (seconds->date EPOCH) "32279")
         (Status 1 (seconds->date EPOCH) "22900")
         (Status 2 (seconds->date (+ 1 EPOCH)) "33116")
         (Status 1 (seconds->date (+ 2 EPOCH)) "22883")))
  
  (check-equal?
   (sensor-statuses 1)
   (list (Status 1 (seconds->date EPOCH) "22900")
         (Status 1 (seconds->date (+ 2 EPOCH)) "22883")))
  
  )
