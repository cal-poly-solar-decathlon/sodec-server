#lang racket

;; populate a db with mock data for about 60 days

(require db/base
         db/mysql
         #;db/sqlite3
         racket/runtime-path
         racket/date
         "../device-descriptions.rkt"
         "../mysql-socket.rkt")

(module* test racket/base)



(date-display-format 'iso-8601)

(define-runtime-path here ".")

;; create a connection to the new database
(define nuconn (mysql-connect #:database "sodec3"
                              #:user "clements"
                              #:password "aoeuidht"
                              #:socket mysql-socket
                              ))

(define DAY-SECONDS 86400)
(define start-seconds (find-seconds 0 0 0 1 1 2015))
(define end-seconds (+ start-seconds 3600 #;(* 60 DAY-SECONDS)))


(define ELEC-READING-INTERVAL 15)
(define ELEC-READING-RANGE (expt 2 30))
;; temperature reading every 4 electrical readings
(define TEMP-READING-MODULO 4)
(define TEMP-HUM-READING-RANGE 600)



(define elec-devices (some-devices #px"^s-elec-"))
(define temp-hum-devices (append (some-devices #px"^s-temp-")
                                 (some-devices #px"^s-hum-")))



(random-seed 1434758162)

(for ([i (in-naturals)]
      [t (in-range start-seconds end-seconds ELEC-READING-INTERVAL)])
  (define timestamp (date->string (seconds->date t) #t))
  (for ([device (in-list elec-devices)])
    (query-exec nuconn
                "INSERT INTO sensorevents VALUES (DEFAULT,?,?,?)"
                device
                timestamp
                (random ELEC-READING-RANGE)))
  (when (= 0 (modulo i TEMP-READING-MODULO))
    (for ([device (in-list temp-hum-devices)])
      (query-exec nuconn
                  "INSERT INTO sensorevents VALUES (DEFAULT,?,?,?)"
                  device
                  timestamp
                  (random ELEC-READING-RANGE)))))


