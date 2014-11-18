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

;; adding as needed...
(define-type SensorKind 
  (U "temperature-sensor"
     "occupancy-sensor"))

(define-predicate sensor-kind? SensorKind)


;; a kind is one of
;; - 'temperature-sensor
;; - 'occupancy-sensor
;; - 'ambient-light-sensor
;; - 'ammeter
;; - 'dimmable-light-control
;; - 'on-off-light-control


(define-runtime-path here ".")

(define conn (sqlite3-connect #:database (build-path here "sodec.db")))

(define (create-db)
  (sqlite3-connect #:database (build-path here "sodec.db")
                   #:mode 'create))

(: testing? (Parameterof Boolean))
(define testing? 
  (make-parameter #f 
                  (lambda (b)
                    (cond [(boolean? b) b]
                          [else (testing?)]))))

(define (devices-table)
  (cond [(testing?) "test_devices"]
        [else "devices"]))
(define (statuses-table)
  (cond [(testing?) "test_statuses"]
        [else "statuses"]))

;; NB: sqlite doesn't do ... well, nearly every kind of type.
;; I'm just using "text" to represent dates, because that's what we've got...
;; EXTREMELY DANGEROUS. DESTROYS ALL DATA
(define (reset-database!)
  (query-exec conn "DROP TABLE devices")
  (query-exec conn "CREATE TABLE devices (id INTEGER PRIMARY KEY AUTOINCREMENT, kind TEXT, name TEXT)")
  (query-exec conn "DROP TABLE statuses")
  (query-exec conn "CREATE TABLE statuses (id INTEGER, date TEXT, value TEXT)"))

;; create temporary tables for testing
(define (create-database-test-tables!)
  (query-exec conn "CREATE TEMPORARY TABLE test_devices (id INTEGER PRIMARY KEY AUTOINCREMENT, kind TEXT, name TEXT)")
  (query-exec conn "CREATE TEMPORARY TABLE test_statuses (id INTEGER, date TEXT, value TEXT)"))

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

(: ensure-nat (Any -> Natural))
(define (ensure-nat val)
  (cond [(exact-nonnegative-integer? val) val]
        [else (raise-argument-error 
               'ensure-nat
               "exact nonnegative integer"
               0 val)]))

;; record a sensor reading
(define (add-sensor-reading! id status)
  (define date (current-seconds))
  (query-exec conn
              "INSERT INTO statuses VALUE (?,?,?)"
              id date status))


(parameterize ([testing? #t])
  (check-equal? 
   (add-device! "temperature-sensor" "Living Room Temperature Sensor")
   1)
  
  (check-equal? 
   (add-device! "temperature-sensor" "Bedroom Temperature Sensor")
   2)
  
  (check-equal? 
   (all-devices)
   (list (Device 1 "temperature-sensor" "Living Room Temperature Sensor")
         (Device 2 "temperature-sensor" "Bedroom Temperature Sensor")))
  )
