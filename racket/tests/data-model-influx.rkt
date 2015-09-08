#lang racket
 
(require rackunit
         rackunit/text-ui
         "../data-model-influx.rkt"
         racket/date)


;; safe to do this every time...
(reset-database-test-tables!)


(define (string-or-null? s)
  (or (string? s)
      (equal? s 'null)))


#;(parameterize ([testing? #t])
  
  (check-not-exn (lambda () (record-sensor-status! "temperature" "outside" 9)))
  (check-equal? (sensor-latest-event "temperature" "outside") 9)
  )

(run-tests
(test-suite
 "data model tests"
(parameterize ([testing? #t])
  
  (define (se->dr e)
    (cond [(eq? #f e) (error 'se->dr "test fail")]
          [else (list (sensor-event-device e)
                      (sensor-event-reading e))]))

  (check-exn #px"expected: legal measurement name"
             (lambda () (record-sensor-status! "shmoovy" "blaggo" 9)))
  (check-exn #px"expected: legal location for measurement"
             (lambda () (record-sensor-status! "electricity_used" "blaggo" 9)))

  (check-not-exn
   (lambda () (record-sensor-status! "temperature" "living_room" 322)))
  
  ;; this is the latest:
  (check-equal? (sensor-latest-reading "temperature" "living_room")
                322)

  ;; no readings for outside yet
  (check-equal? (sensor-latest-reading "temperature" "outside")  #f)
  
  (record-sensor-status! "temperature" "bedroom" 229)
  
  ;; this is still the latest:
  (check-equal? (sensor-latest-reading "temperature" "living_room")
                322)
  
  (sleep 1.5)
  (record-sensor-status! "temperature" "living_room" 331)
  
  ;; now the latest has changed:
  (check-equal? (sensor-latest-reading "temperature" "living_room")
                331)

  (sleep 1.5)
  (record-sensor-status! "temperature" "bedroom" 228)
 
  (check-equal? (maybe-reading->jsexpr
                 (sensor-latest-reading "temperature" "living_room"))
                331)
  
  #;(check-equal?
   (map se->dr (sensor-events "s-temp-bed"))
   (list (list "s-temp-bed" 229)
         (list "s-temp-bed" 228)))

    #;(
  


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
                                           229])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts2)
                                           228])))
  
  (record-sensor-status! "s-temp-bed" 224)
  
  (check-match (sensor-events-in-range "s-temp-bed" ts-1 ts+1sec)
               (list (struct SensorEvent ["s-temp-bed"
                                           (? date? ts1)
                                           229])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts2)
                                           228])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts3)
                                           224])))

  (check-equal? (count-sensor-events-in-range "s-temp-bed" ts-1 ts+1sec)
                3)
  
  (check-match
   (events->jsexpr/short (sensor-events-in-range "s-temp-bed" ts-1 ts+1sec))
   (hash-table ('baseTimestamp (? number? n))
               ('baseStatus 229)
               ('seriesData 
                (list (list (? number? n1) -1) (list (? number? n2) -4)))))


  ;; list-devices
  (test-case
   "devices-list"
   (check-pred (lambda (devlist)
                 (and (list? devlist)
                      (for/and ([ht (in-list devlist)])
                        (match ht
                          [(hash-table ('device (? string? dev))
                                       ('description (? string? descn))) #t]
                          [other #f]))
                      (< 10 (length devlist))))
               (devices-list))))

  )))