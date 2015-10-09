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

;; start a loop that pings every so often
(define (start-ip-ping #:host host #:port port)
  (define the-url (ping-url host port))
  (thread 
   (lambda ()
     (let loop ()
       (thread
        (lambda ()
          (send-ping the-url)))
       (sleep IP-PING-SLEEP-INTERVAL)
       (loop)))))

;; build the ping url
(define (ping-url host port)
  (url "http" #f ;; user
       host port
       #t ;; absolute?
       (for/list ([element ENDPOINT])
         (path/param element '()))
       null #f ;;fragment
       ))

;; send a heartbeat to the url
(define (send-ping the-url)
  (with-handlers ([(lambda (exn) #t)
                   (lambda (exn)
                     (log-sodec-error
                      "~a : error during ip ping:\n~a"
                      (date->string (seconds->date
                                     (current-seconds))
                                    #t)
                      (exn-message exn)))])
    (define-values (status-line headers body-port)
      (http-sendrecv/url the-url))
    ;; drain the body port just to be civilized:
    (regexp-match #px".*" body-port)))


(module+ test
  (require rackunit)
  (test-equal? "send-ping"
               (send-ping (ping-url "localhost"
                                    90872))
               (void)))



