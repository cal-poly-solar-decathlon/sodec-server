#lang racket/base

(require net/url
         racket/contract
         racket/date)

(define-logger sodec)

(date-display-format 'iso-8601)

(provide (contract-out
          [start-ip-ping (-> #:host string?
                                    #:port exact-integer?
                                    thread?)]))

(define ENDPOINT (list "ip-ping" "index.html"))

;; ping every 15 minutes
(define IP-PING-SLEEP-INTERVAL (* 60 15))

(define (start-ip-ping #:host host #:port port)
  (define url
    (url "http" #f ;; user
     host port
     #t ;; absolute?
     (for/list ([element ENDPOINT])
       (path/param element '()))
     null #f ;;fragment
     ))

  (thread 
   (lambda ()
     (let loop ()
       (thread
        (lambda ()
          (with-handlers ([(lambda (exn) #t)
                           (lambda (exn)
                             (log-sodec-error
                              "~a : error during ip ping:\n~a"
                              (date->string (seconds->date
                                             (current-seconds))
                                            #t)
                              (exn-message exn)))])
            (define-values (status-line headers body-port) (http-sendrecv/url url))
            ;; drain the body port just to be civilized:
            (regexp-match #px".*" body-port))))
       (sleep IP-PING-SLEEP-INTERVAL)
       (loop)))))






