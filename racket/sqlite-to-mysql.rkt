#lang racket


;; port all the data from the sqlite database to the mysql database

(require db/base
         db/mysql
         db/sqlite3
         racket/runtime-path
         racket/date)



(date-display-format 'iso-8601)

(define-runtime-path here ".")

(define oldconn (sqlite3-connect #:database (build-path here "sodec.db")
                                 #:mode 'create))

;; create a connection to the new database
(define nuconn (mysql-connect #:database "sodec"
                              #:user "clements"
                              #:password "aoeuidht"
                              ))

(define (query-old)
  (time
   (query-rows oldconn
               {~a "SELECT * FROM events WHERE ID=? AND timestamp = "
                   "(SELECT MAX(timestamp) FROM events WHERE ID=?);"}
               
               "s-temp-kit"
               "s-temp-kit"
               )))

(define (query-new)
  (time
   (query-rows nuconn
               {~a "SELECT * FROM sensorevents WHERE ID=? AND timestamp = "
                   "(SELECT MAX(timestamp) FROM sensorevents WHERE ID=?);"}
               
               "s-temp-bed"
               "s-temp-bed"
               )))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(query-new)
(query-new)
(query-new)
(query-old)
(query-old)
(query-old)




#;(for ([(id timestamp value) (in-query oldconn "SELECT * FROM events"
                                      #:fetch 100)]
      [i (in-naturals)])
  (when (= 0 (modulo i 1000))
    (printf "i = ~v\n" i))
  (query-exec nuconn
              "INSERT INTO sensorevents VALUE (DEFAULT,?,?,?)"
              "s-temp-bed"
              (date->string (seconds->date timestamp) #t)
              (string->number value)))

#;(define events (sensor-events "s-temp-kit"))

#;(for ([e (in-list events)])
  (query-exec nuconn
              "INSERT INTO events VALUES (?, ?, ?, ?)"))

