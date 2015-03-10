#lang racket

(require rackunit
         "../data-model.rkt"
         racket/date)


;; safe to do this every time...
(reset-database-test-tables!)

(parameterize ([testing? #t])
  
  (define (se->dr e)
    (cond [(eq? #f e) (error 'se->dr "test fail")]
          [else (list (SensorEvent-device e)
                      (SensorEvent-reading e))]))
  
  (check-not-exn
   (lambda () (record-sensor-status! "s-temp-lr" 32279)))
  
  ;; this is the latest:
  (check-equal? (se->dr (sensor-latest-event "s-temp-lr"))
                (list "s-temp-lr" 32279))

  (record-sensor-status! "s-temp-bed" 22900)
  
  ;; this is still the latest:
  (check-equal? (se->dr (sensor-latest-event "s-temp-lr"))
                (list "s-temp-lr" 32279))
  
  (sleep 1.5)
  (record-sensor-status! "s-temp-lr" 33116)
  
  ;; now the latest has changed:
  (check-equal? (se->dr (sensor-latest-event "s-temp-lr"))
                (list "s-temp-lr" 33116))

  (sleep 1.5)
  (record-sensor-status! "s-temp-bed" 22883)
 
  (check-match (maybe-event->jsexpr (sensor-latest-event "s-temp-lr"))
               (hash-table ('device-id "s-temp-lr")
                           ('timestamp (? number? n))
                           ('status 33116)))
  
  (check-equal?
   (map se->dr (sensor-events "s-temp-bed"))
   (list (list "s-temp-bed" 22900)
         (list "s-temp-bed" 22883)))
  
  (check-not-exn (lambda () (current-timestamp)))
  
  (define ts (current-timestamp))
  (define ts+1sec (seconds->date (+ (date->seconds ts) 1)))
  (define ts-1 (seconds->date (- (date->seconds ts) 86400)))
  (define ts-2 (seconds->date (- (date->seconds ts) (* 2 86400))))
  
  (check-equal? (sensor-events-in-range "s-temp-bed" ts-2 ts-1)
                '())
  (check-match (sensor-events-in-range "s-temp-bed" ts-1 ts+1sec)
                (list (struct SensorEvent ["s-temp-bed"
                                           (? date? ts1)
                                           22900])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts2)
                                           22883])))
  
  )
