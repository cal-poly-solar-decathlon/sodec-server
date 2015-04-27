#lang racket

(require "../generate-sensor-names.rkt"
         "../mysql-socket.rkt"
         db
         racket/runtime-path)


(define conn
  (virtual-connection
   (connection-pool
    (lambda ()
      (mysql-connect #:user "clements"
                     #:database "sodec2"
                     #:password "aoeuidht"
                     #:socket mysql-socket
                     )))))

(define-runtime-path here ".")
(define API-string (file->string (build-path here ".." "apiary.apib")))
(define API-paragraphs (regexp-split #px"\n\n" API-string))
(unless (string=? (list-ref API-paragraphs 7)
                  "### List of IDs")
  (error 'api-parsing "expected ### List of IDs for eighth para, got: "
         (list-ref API-paragraphs 7)))
(define description-text (list-ref API-paragraphs 8))

;; pairs of device id and description:
(define dd-pairs
  (for/list ([l (regexp-split #px"\n" description-text)])
  (match (regexp-match #px"^- `([^`]+)`[ ]+\\(([^\\)]+)\\)" l)
    [(cons _ rest) rest]
    [other (error 'description-parsing
                  "parse failure for line: ~e"
                  l)])))

(for ([dd-pair (in-list dd-pairs)])
  (match-define (list device description) dd-pair)
  (query-exec conn
              "UPDATE devices SET description=? WHERE name=?;"
              description
              device))
