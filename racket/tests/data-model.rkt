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

(define (ms->s/floor n)
  (inexact->exact (floor (/ n 1000))))

(define (ms->s/ceiling n)
  (inexact->exact (ceiling (/ n 1000))))

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

  (define now1 (inexact->exact (round (current-inexact-milliseconds))))
  (define ts+1sec (+ now1 1000))
  (define ts+2sec (+ now1 2000))
  (define ts+4sec (+ now1 4000))
  (define ts-1day (- now1 86400000))
  (define ts-2days (- now1 (* 2 1000 86400)))
  
  (record-device-status! "temperature" "bedroom" 229)
  
  ;; this is still the latest:
  (test-equal?
   "still-the-latest"
   (device-latest-reading "temperature" "living_room")
   322)

  ;; record something in the future!
  (define future1 (+ now1 500))
  (record-device-status! "temperature" "living_room" 331
                         #:timestamp future1) 
  (sleep 1)
  
  
  
  ;; now the latest has changed:
   (check-equal? (device-latest-reading "temperature" "living_room")
                331)

  (define future2 (+ now1 800))
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
                           (ms->s/floor (- now1 1000))
                           (ms->s/ceiling future2))
   (list (event future1 229)
         (event future2 228))))

  ;; you can record random electric devices...
  (check-not-exn
   (lambda ()
     (record-device-status! "electric_power" "brungy_wungy"
                             11982)))

  

  (check-not-exn (lambda () (current-timestamp)))
  

  (check-exn #px"expected: \\(integer-in"
             (λ() (device-events-in-range "temperature" "bedroom" ts-2days ts-1day)))
  
  (check-equal? (device-events-in-range "temperature" "bedroom"
                                        (ms->s/floor ts-2days)
                                        (ms->s/floor ts-1day))
                '())

  
  (check-match (device-events-in-range "temperature" "bedroom"
                                       (ms->s/floor now1)
                                       (ms->s/ceiling ts+1sec))
                (list (struct event [(? exact-integer? ts1)
                                           229])
                      (struct event [(? exact-integer? ts2)
                                           228])))

  
  (record-device-status! "temperature" "bedroom" 224)

  (sleep 1)
  (check-equal? (count-device-events-in-range "temperature" "bedroom"
                                              (ms->s/floor ts-1day)
                                              (ms->s/ceiling ts+4sec))
                3)

  
  (check-match
   (datapoints->jsexpr (device-events-in-range "temperature" "bedroom"
                                           (ms->s/floor ts-1day)
                                           (ms->s/ceiling ts+4sec)))
   (list (hash-table ('t (? exact-integer? n1))
                     ('r 229))
         (hash-table ('t (? exact-integer? n2))
                     ('r 228))
         (hash-table ('t (? exact-integer? n3))
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
    ;; ARRGGHH. Finally figured out influxdb forcibly rounds its
    ;; interval starts. so, if you ask for 5-second intervals starting
    ;; at 14:31, it will actually start at 14:30, because 14:30 is
    ;; 'divisible by 5'. Sounds frightening.
    ;; in order to test this, then, we need times that are "divisible by
    ;; 5 seconds."
    (define ts (find-seconds 35 04 17 15 09 2015))
    (define (secs n) (+ ts n))

    (define testpoints
      '((1 33)
        (0 11)
        (-1 24)
        (-3 36)
        (-4 99)))
    (for ([t (in-list testpoints)])
      (record-device-status! "temperature" "kitchen" (cadr t) #:timestamp
                             (* (secs (car t)) 1000)))
    
    (check-equal? (device-interval-means "temperature" "kitchen"
                                         (secs -8) (secs 2) 5)
                  (list (summary (* 1000 (- ts 10)) #f)
                        (summary (* 1000 (- ts 5)) (/ (+ 99 36 24) 3))
                        (summary (* 1000 ts) 22)))
    )

  )))
