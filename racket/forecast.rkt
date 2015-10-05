#lang racket/base

(require net/url
         racket/contract
         racket/date
         json
         "forecast-example.rkt"
         "time-contracts.rkt")

(define-logger sodec)

(provide
 (contract-out
  [latest-forecast (-> (or/c false? jsexpr?))]
  [latest-forecast-timestamp-ms (-> (or/c false? ts-milliseconds?))]
  [start-forecast-monitor (-> thread?)]
  [use-fake-forecast! (-> void?)]))

(define FORECAST-SLEEP-INTERVAL (* 20 60))

(define (start-forecast-monitor)
  (thread 
   (lambda ()
     (let loop ()
       (thread
        (lambda ()
          (with-handlers ([(lambda (exn) #t)
                           (lambda (exn)
                             (log-sodec-error
                              "~a : error during forecast fetch:\n~a"
                              (date->string (seconds->date
                                             (current-seconds))
                                            #t)
                              (exn-message exn)))])
            (update-stored-forecast!))))
       (sleep FORECAST-SLEEP-INTERVAL)
       (loop)))))
(define (false? v) (eq? v #f))

(define stored-forecast #f)
(define stored-timestamp #f)

(define FORECAST-API-KEY "7ae8da03eeb40276c639d6591e1f9610")
(define COMPETITION-LAT 33.673676)
(define COMPETITION-LON -117.741895)

(define forecast-url
  (url
   "https"
   #f
   "api.forecast.io"
   #f
   #t
   (list
    (path/param "forecast" '())
    (path/param FORECAST-API-KEY '())
    (path/param (format "~a,~a"
                        COMPETITION-LAT
                        COMPETITION-LON) '()))
   '((units . "si"))
   #f))

;; return the latest stored forecast
(define (latest-forecast)
  stored-forecast)

;; return the timestamp of the latest stored forecast
;; (yes, small possible race here)
(define (latest-forecast-timestamp-ms)
  stored-timestamp)

(define (update-stored-forecast!)
  (define-values (status headers body-port)
    (http-sendrecv/url forecast-url))
  (unless (regexp-match #px"^HTTP/1.1 200" status)
    (error 'update-stored-forecast
           "expected 200 OK response, got ~e, with body ~e"
           status
           (regexp-match #px".*" body-port)))
  (set! stored-forecast (read-json body-port))
  (set! stored-timestamp
        (inexact->exact (floor (current-inexact-milliseconds)))))

(define (use-fake-forecast!)
  (set! stored-forecast example-forecast)
  (set! stored-timestamp example-forecast-ms))
