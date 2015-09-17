#lang racket
 
(require rackunit
         rackunit/text-ui
         "../data-model.rkt"
         racket/date)


;; safe to do this every time...
(reset-database-test-tables!)


(define (string-or-null? s)
  (or (string? s)
      (equal? s 'null)))


#;(parameterize ([testing? #t])
  
  (check-not-exn (lambda () (record-device-status! "temperature" "outside" 9)))
  (check-equal? (device-latest-event "temperature" "outside") 9)
  )

(run-tests
(test-suite
 "data model tests"
(parameterize ([testing? #t])

  (test-case
   "record-device-status illegal names"
   (check-exn #px"expected: measurement?"
              (lambda () (record-device-status! "shmoovy" "blaggo" 9)))
   (check-exn #px"expected: device?"
              (lambda () (record-device-status! "electric_power" "bla ggo" 9))))

  (test-not-exn
   "record-device-status"
   (lambda () (record-device-status! "temperature" "living_room" 322)))
  
  ;; this is the latest:
  (check-equal? (device-latest-reading "temperature" "living_room")
                322)

  ;; no readings for outside yet
  (check-equal? (device-latest-reading "temperature" "outside")  #f)

  (define now1 (current-seconds))
  (record-device-status! "temperature" "bedroom" 229)
  
  ;; this is still the latest:
  (test-equal?
   "still-the-latest"
   (device-latest-reading "temperature" "living_room")
   322)

  ;; record something in the future!
  ;; NB: "current-seconds" may be as much as a second in the past...
  (define future1 (+ (* 1000 now1) 1500))
  (record-device-status! "temperature" "living_room" 331
                         #:timestamp future1) 
  (sleep 2)
  
  
  
  ;; now the latest has changed:
   (check-equal? (device-latest-reading "temperature" "living_room")
                331)

  (define future2 (+ (* 1000 now1) 1800))
  (record-device-status! "temperature" "bedroom" 228
                         #:timestamp future2)

  (sleep 1)
 
  (check-equal? (maybe-reading->jsexpr
                 (device-latest-reading "temperature" "living_room"))
                331)

  (test-case
   "device-events-in-range"
  (check-match
   (device-events-in-range "temperature" "bedroom"
                           (sub1 now1)
                           (add1 (round (/ future2 1000))))
   (list (event future1 229)
         (event future2 228))))

  ;; you can record random electric devices...
  (check-not-exn
   (lambda ()
     (record-device-status! "electric_power" "brungy_wungy"
                             11982)))

  

  (check-not-exn (lambda () (current-timestamp)))
  
  (define ts+1sec (+ now1 1))
  (define ts+2sec (+ now1 2))
  (define ts-1 (- now1 86400))
  (define ts-2 (- now1 (* 2 86400)))
  
  (check-equal? (device-events-in-range "temperature" "bedroom" ts-2 ts-1)
                '())

  
  (check-match (device-events-in-range "temperature" "bedroom" ts-1 ts+1sec)
                (list (struct event [(? exact-integer? ts1)
                                           229])
                      (struct event [(? exact-integer? ts2)
                                           228])))

  
  
  (record-device-status! "temperature" "bedroom" 224)
  
  #;(check-match (device-events-in-range "s-temp-bed" ts-1 ts+1sec)
               (list (struct SensorEvent ["s-temp-bed"
                                           (? date? ts1)
                                           229])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts2)
                                           228])
                      (struct SensorEvent ["s-temp-bed"
                                           (? date? ts3)
                                           224])))

  (sleep 1)
  (check-equal? (count-device-events-in-range "temperature" "bedroom" ts-1 ts+1sec)
                3)

  
  (check-match
   (events->jsexpr (device-events-in-range "temperature" "bedroom" ts-1 ts+1sec))
   (list (hash-table ('t (? exact-integer? n1))
                     ('r 229))
         (hash-table ('t (? exact-integer? n1))
                     ('r 228))
         (hash-table ('t (? exact-integer? n1))
                     ('r 224))))


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

  #;(record-device-status! )
  (let ()
    (define ts (current-timestamp))
    (define (secs n) (+ ts n))

    (define testpoints
      '((0 10)
        (-1 24)
        (-3 36)
        (-4 100)))
    (for ([t (in-list testpoints)])
      (record-device-status! "temperature" "kitchen" (cadr t) #:timestamp (* (secs (car t)) 1000)))

    (print (device-interval-means "temperature" "kitchen"
                                  (secs -10) (secs 0) 5))
    
    (check-equal? (device-interval-means "temperature" "kitchen" (secs -10) (secs 0) 5)
                  (list "no event"
                        (/ (+ 100 36 24) 3)))
    )

  )))
