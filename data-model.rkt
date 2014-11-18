#lang typed/racket

(require racket/runtime-path
         typed/rackunit)

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


;; a device is (make-device id kind)
(struct Device ([id : Natural]
                [kind : SensorKind]
                [name : String])
  #:transparent)

(struct Status ([id : Natural]
                [timestamp : Natural]
                [status : String])
  #:transparent)

;; adding as needed...
(define-type SensorKind 
  (U "continuous-sensor"
     "on-off-sensor"
))

(define-predicate sensor-kind? SensorKind)

(define-runtime-path here ".")

(define conn (sqlite3-connect #:database (build-path here "sodec.db")))

(define (create-db)
  (sqlite3-connect #:database (build-path here "sodec.db")
                   #:mode 'create))

;; are we in testing mode (i.e., using testing tables)?
(: testing? (Parameterof Boolean))
(define testing? 
  (make-parameter #f 
                  (lambda (b)
                    (cond [(boolean? b) b]
                          [else (testing?)]))))

(define (devices-table)
  (cond [(testing?) "test_devices"]
        [else "devices"]))
(define (status-table)
  (cond [(testing?) "test_status"]
        [else "status"]))

;; NB: sqlite doesn't do ... well, nearly every kind of type.
;; I'm just using "text" to represent dates, because that's what we've got...
;; EXTREMELY DANGEROUS. DESTROYS ALL DATA
(define (reset-database!)
  (query-exec conn "DROP TABLE devices")
  (query-exec conn "CREATE TABLE devices (id INTEGER PRIMARY KEY AUTOINCREMENT, kind TEXT, name TEXT)")
  (query-exec conn "DROP TABLE status")
  (query-exec conn "CREATE TABLE status (id INTEGER, timestamp INTEGER, value TEXT)"))

;; create temporary tables for testing
(define (create-database-test-tables!)
  (query-exec conn "CREATE TEMPORARY TABLE test_devices (id INTEGER PRIMARY KEY AUTOINCREMENT, kind TEXT, name TEXT)")
  (query-exec conn "CREATE TEMPORARY TABLE test_status (id INTEGER, timestamp INTEGER, value TEXT)"))

;; safe to do this every time...
(create-database-test-tables!)

;; return all known devices
(: all-devices (-> (Listof Device)))
(define (all-devices)
  (map row->device
       (query-rows conn (~a "SELECT * FROM "(devices-table)))))

(: row->device ((Vectorof Any) -> Device))
(define (row->device row)
  (match row
    [(vector (? exact-nonnegative-integer? id)
             (? sensor-kind? kind)
             (? string? name))
     (Device id kind name)]
    [other
     (raise-argument-error 'row->device
                           "vector containing nat, kind, name"
                           0
                           row)]))

(: row->status ((Vectorof Any) -> Status))
(define (row->status row)
  (match row
    [(vector (? exact-nonnegative-integer? id)
             (? exact-nonnegative-integer? timestamp)
             (? string? status))
     (Status id timestamp status)]
    [other
     (raise-argument-error 'row->status
                           "vector containing nat, nat, string"
                           0
                           row)]))

;; add a new device with given name and kind
(: add-device! (SensorKind String -> Natural))
(define (add-device! kind name)
  ;; there is a possible race condition here. I guess 
  ;; I have to make this code single-threaded?
  (query-exec conn
              (~a "INSERT INTO "(devices-table)" VALUES (NULL,?,?)")
              kind
              name)
  (ensure-nat (query-value conn 
                           (~a "SELECT max(id) FROM "(devices-table)))))

;; find a device with a given name. Ensure there's only one.
(: find-device-by-name (String -> Natural))
(define (find-device-by-name name)
  (define matching-devices
    (map row->device
         (query-rows (~a "SELECT * FROM "(devices-table)" WHERE name=?")
                     name)))
  (match matching-devices
    [(list d) (Device-id d)]
    [(list) (error )]))

(: ensure-nat (Any -> Natural))
(define (ensure-nat val)
  (cond [(exact-nonnegative-integer? val) val]
        [else (raise-argument-error 
               'ensure-nat
               "exact nonnegative integer"
               0 val)]))

;; record a sensor reading
(: add-sensor-status! (Natural String -> Void))
(define (add-sensor-status! id status)
  (define timestamp (current-seconds))
  (query-exec conn
              (~a "INSERT INTO " (status-table) " VALUES (?,?,?)")
              id timestamp status))

;; return all sensor statuses (from all times, all sensors)
(: all-statuses (-> (Listof Status)))
(define (all-statuses)
  (map row->status
       (query-rows conn 
                   (~a "SELECT * FROM "(status-table)" ORDER BY timestamp"))))

;; return all sensor statuses (from all times, one sensor)
(: sensor-statuses (Natural -> (Listof Status)))
(define (sensor-statuses id)
  (map row->status 
       (query-rows conn 
                   (~a "SELECT * FROM "(status-table)" WHERE ID=? ORDER BY timestamp")
                   id)))


(parameterize ([testing? #t])
  (check-equal? 
   (add-device! "continuous-sensor" "Living Room Temperature Sensor")
   1)
  
  (check-equal? 
   (add-device! "continuous-sensor" "Bedroom Temperature Sensor")
   2)
  
  (check-equal? 
   (all-devices)
   (list (Device 1 "continuous-sensor" "Living Room Temperature Sensor")
         (Device 2 "continuous-sensor" "Bedroom Temperature Sensor")))
  
  (check-equal?
   (all-statuses)
   (list))
  
  (define ts1 (current-seconds))
  (define ts1-pos (cond [(< ts1 0) (error 'ts1-pos "ts1 not positive!")]
                        [else ts1]))
  
  (add-sensor-status! 2 (number->string 32.2798))
  (sleep 2)
  (add-sensor-status! 1 (number->string 22.9))
  (sleep 1)
  (add-sensor-status! 2 (number->string 33.1164))
  
  (check-equal?
   (all-statuses)
   (list (Status 2 ts1-pos "32.2798")
         (Status 1 (+ 2 ts1-pos) "22.9")
         (Status 2 (+ 3 ts1-pos) (number->string 33.1164))))
  
  (check-equal?
   (sensor-statuses 1)
   (list (Status 1 (+ 2 ts1-pos) "22.9")))
  
  )
