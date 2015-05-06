#lang racket/base

(require "device-descriptions.rkt"
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

(define (add-devices)
  (for ([dd-pair (in-list dd-pairs)])
    (with-handlers ([(lambda (exn)
                       (and (exn:fail? exn)
                            (regexp-match #px"^query-exec: Duplicate entry "
                                          (exn-message exn))))
                     (lambda (exn)
                       (fprintf (current-error-port)
                                "device ~v already present, ignoring\n"
                                (car dd-pair)))])
      (query-exec conn
                  "INSERT INTO devices VALUE (?,?)"
                  (car dd-pair)
                  (cadr dd-pair)))))

#;()
;; certain cumulative devices need to have at least one initial zero.
#;(define (ensure-at-least-one-reading))
  
#;(query-exec conn
                "INSERT INTO controleventresultcodes VALUE (0)")

#;(for ([dd-pair (in-list dd-pairs)])
  (define device (car dd-pair))
  (define description (cadr dd-pair))
  (query-exec conn
              "UPDATE devices SET description=? WHERE name=?;"
              description
              device))

(add-devices)

#;(define START-TIMESTAMP (sql-timestamp ))