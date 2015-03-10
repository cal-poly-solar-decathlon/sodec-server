#lang racket

(require "generate-sensor-names.rkt"
         db)

(define conn
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:user "clements"
                     #:database "sodec"
                     #:password "aoeuidht"
                     ;;#:port 11306
                     )))))

(define (populate-db)
  (for ([device-name sensor-names])
      (query-exec conn
                  "INSERT INTO devices VALUE (?)"
                  (symbol->string device-name)))
  
  (query-exec conn
                "INSERT INTO controleventresultcodes VALUE (0)"))