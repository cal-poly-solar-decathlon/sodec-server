#lang racket/base

(require "device-readings.rkt"
         "../device-table.rkt"
         "mock-electricity.rkt")

(provide run-mock-temp-hum-elec)

;; the temperature and humidity:

(define TEMP-QUANTA 10)
(define TEMP-ROUNDING (/ 1 TEMP-QUANTA))
(define INITIAL-TEMPERATURE (* 20 TEMP-QUANTA))

;; generate a reading every so many seconds
(define TEMP-READING-SECONDS 60)


;; generate a mock device reading every 'interval' seconds,
;; using a given generator
(define (run-mock-device measurement id interval generator)
  (thread 
   (lambda ()
     (let loop ()
       (thread (lambda () (send-reading! measurement id (generator))))
       (sleep interval)
       (loop)))))

;; a temperature generator
(define (make-temperature-generator)
  (let ([t-box (box INITIAL-TEMPERATURE)])
    (lambda ()
      (set-box! t-box
                (max 0
                     (+ (unbox t-box)
                        (- (random 5) 2))))
      (unbox t-box))))

;; a humidity generator. Wanders a bit more slowly. Can't go
;; outside 0<=h<=100.
(define (make-humidity-generator)
  (let ([h-box (box INITIAL-HUMIDITY)])
    (lambda ()
      (set-box! h-box
                (max 0
                     (min 1000
                          (+ (unbox h-box)
                             (- (random 3) 1)))))
      (unbox h-box))))

;; in tenths of a percent
(define INITIAL-HUMIDITY 664)


;; the ids of the temperature devices

(define temperature-devices
  (filter (lambda (s)
            (not (regexp-match #px"^testing_" s)))
          (hash-ref measurement-device-table "temperature")))

(define humidity-devices
  (filter (lambda (s)
            (not (regexp-match #px"^testing_" s)))
          (hash-ref measurement-device-table "humidity")))


(define (run-mock-temp-hum)
  ;; start temperature threads:
  (for ([device (in-list temperature-devices)])
    (run-mock-device "temperature" device TEMP-READING-SECONDS
                     (make-temperature-generator)))
  ;; start humidity threads:
  (for ([device (in-list humidity-devices)])
    (run-mock-device "humidity" device TEMP-READING-SECONDS
                     (make-humidity-generator))))



(define (run-mock-temp-hum-elec)
  (run-mock-temp-hum)
  (run-mock-electric))