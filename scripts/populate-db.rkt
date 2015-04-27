#lang racket/base

(require "../generate-sensor-names.rkt"
         "../mysql-socket.rkt"
         db)

(define conn
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:user "clements"
                     #:database "sodec2"
                     #:password "aoeuidht"
                     #:socket mysql-socket
                     )))))

(define (populate-db)
  (for ([device-name device-names])
    (with-handlers ([(lambda (exn)
                       (and (exn:fail? exn)
                            (regexp-match #px"^query-exec: Duplicate entry "
                                          (exn-message exn))))
                     (lambda (exn)
                       (fprintf (current-error-port)
                                "device ~v already present, ignoring\n"
                                device-name))])
      (query-exec conn
                  "INSERT INTO devices VALUE (?)"
                  (symbol->string device-name))))
  
  (query-exec conn
                "INSERT INTO controleventresultcodes VALUE (0)"))

(populate-db)

#;(define START-TIMESTAMP (sql-timestamp ))