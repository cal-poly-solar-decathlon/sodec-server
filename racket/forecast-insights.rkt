#lang racket/base

(require racket/contract
         "device-table.rkt"
         (only-in math/statistics mean stddev)
         (only-in racket/list take)
         racket/match
         "forecast.rkt"
         "insight-struct.rkt"
         json)

(provide
 (contract-out
  [forecast-insights (-> (listof insight?))]))


;; insights based on the forecast
(define (forecast-insights)
  (forecast->insights (latest-forecast)))

;; convert a forecast to a list of insights
(define (forecast->insights forecast)
  (cond [(not forecast) (list)]
        [else (join-insights
               (append
                (alert-insights forecast)
                (hi-lo-temp-insights forecast)))]))

;; formulate the forecast's alerts as insights
(define (alert-insights forecast)
  (for/list ([alert (in-list (hash-ref forecast 'alerts null))])
    (insight
     (format
      "~a\n~a"
      (hash-ref alert 'title)
      (car (regexp-split #px"\n" (hash-ref alert 'description))))
     SHOULD-ACT)))


;; insights about predicted highs and lows
(define (hi-lo-temp-insights forecast)
  (match (hash-ref forecast 'hourly #f)
    [#f null]
    [(hash-table ['data hours] [_1 _2] ...)
     (cond [(<= 24 (length hours))
            (define next-24-hours (take hours 24))
            
            (define temperatures
              (filter (λ(x)x)
                      (for/list ([hour (in-list next-24-hours)])
                        (hash-ref hour 'temperature #f))))
            (cond [(null? temperatures) null]
                  [else
                   (list (insight
                          (format "expected high temperature over next \
24 hours: ~a"
                                  (temp-format (apply max temperatures)))
                          25)
                         (insight
                          (format "expected low temperature over next \
24 hours: ~a"
                                  (temp-format (apply min temperatures)))
                          25))])]
           [else null])]))


(module+ test
  (require rackunit)

  (use-fake-forecast!)
  (forecast->insights (latest-forecast)))