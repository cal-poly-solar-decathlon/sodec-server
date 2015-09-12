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

  (test-case
   "record-sensor-status illegal names"
   (check-exn #px"expected: measurement?"
              (lambda () (record-sensor-status! "shmoovy" "blaggo" 9)))
   (check-exn #px"expected: device?"
              (lambda () (record-sensor-status! "electric_power" "bla ggo" 9))))

  (test-not-exn
   "record-sensor-status"
   (lambda () (record-sensor-status! "temperature" "living_room" 322)))
  
  ;; this is the latest:
  (check-equal? (sensor-latest-reading "temperature" "living_room")
                322)

  ;; no readings for outside yet
  (check-equal? (sensor-latest-reading "temperature" "outside")  #f)

  (define now1 (current-seconds))
  (record-sensor-status! "temperature" "bedroom" 229)
  
  ;; this is still the latest:
  (test-equal?
   "still-the-latest"
   (sensor-latest-reading "temperature" "living_room")
   322)

  ;; record something in the future!
  (define future1 (+ (* 1000 (current-seconds)) 500))
  (record-sensor-status! "temperature" "living_room" 331
                         #:timestamp future1) 

  (sleep 1)
  
  ;; now the latest has changed:
   (check-equal? (sensor-latest-reading "temperature" "living_room")
                331)

  (define future2 (+ (* 1000 (current-seconds)) 800))
  (record-sensor-status! "temperature" "bedroom" 228
                         #:timestamp future2)

  (sleep 1)
 
  (check-equal? (maybe-reading->jsexpr
                 (sensor-latest-reading "temperature" "living_room"))
                331)

  (test-case
   "sensor-events-in-range"
  (check-match
   (sensor-events-in-range "temperature" "bedroom"
                           (sub1 now1)
                           (add1 (round (/ future2 1000))))
   (list (event future1 229)
         (event future2 228))))

  ;; you can record random electric devices...
  (check-not-exn
   (lambda ()
     (record-sensor-status! "electric_power" "brungy_wungy"
                             11982)))

  

  (check-not-exn (lambda () (current-timestamp)))
  
  (define ts (current-timestamp))
  (define ts+1sec (+ ts 1))
  (define ts-1 (- ts 86400))
  (define ts-2 (- ts (* 2 86400)))
  
  (check-equal? (sensor-events-in-range "temperature" "bedroom" ts-2 ts-1)
                '())

  
  (check-match (sensor-events-in-range "temperature" "bedroom" ts-1 ts+1sec)
                (list (struct event [(? exact-integer? ts1)
                                           229])
                      (struct event [(? exact-integer? ts2)
                                           228])))

  
  
  (record-sensor-status! "temperature" "bedroom" 224)
  
  #;(check-match (sensor-events-in-range "s-temp-bed" ts-1 ts+1sec)
               (list (struct SensorEvent ["s-temp-bed"
                                           (? date? ts1)
                                           229])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts2)
                                           228])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts3)
                                           224])))

  (check-equal? (count-sensor-events-in-range "temperature" "bedroom" ts-1 ts+1sec)
                3)

  
  #;(check-match
   (events->jsexpr/short (sensor-events-in-range "s-temp-bed" ts-1 ts+1sec))
   (hash-table ('baseTimestamp (? number? n))
               ('baseStatus 229)
               ('seriesData 
                (list (list (? number? n1) -1) (list (? number? n2) -4)))))


  ;; list-devices
  #;(test-case
   "devices-list"
   (check-pred (lambda (devlist)
                 (and (list? devlist)
                      (for/and ([ht (in-list devlist)])
                        (match ht
                          [(hash-table ('device (? string? dev))
                                       ('description (? string? descn))) #t]
                          [other #f]))
                      (< 10 (length devlist))))
               (devices-list)))

  
  
    

  )))
