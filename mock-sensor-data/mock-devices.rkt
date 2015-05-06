#lang racket/base

(require "device-readings.rkt"
         "../device-descriptions.rkt")

(provide run-mock-temp-hum-elec)

;; the temperature and humidity:

(define TEMP-QUANTA 10)
(define TEMP-ROUNDING (/ 1 TEMP-QUANTA))
(define INITIAL-TEMPERATURE (* 20 TEMP-QUANTA))

;; generate a mock sensor reading every 'interval' seconds,
;; using a given generator
(define (run-mock-device id interval generator)
  (thread 
   (lambda ()
     (let loop ()
       (thread (lambda () (send-reading! id (generator))))
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

;; an electrical use generator. only goes up.

(define (make-electrical-use-generator init)
  (let ([saved (box init)])
    (lambda ()
      (set-box! saved
                (+ (unbox saved)
                   (random ELEC-USE-CEILING)))
      (unbox saved))))

;; a really big device would use 1500 watts, which would
;; be 1500 watt-hours per hour, or 1,500,000 mWh per hour,
;; or 25,000 mWh per minute, or 6250 mWh per fifteen seconds.
;; we're going to do a terrible approximation at first
;; and just pick a random number from 1 to 5000.

(define ELEC-USE-CEILING 5000)

;; an electrical generator generator. only goes up:

(define (make-electrical-generation-generator init)
  (let ([saved (box init)])
    (lambda ()
      (set-box! saved
                (+ (unbox saved)
                   (random ELEC-GEN-CEILING)))
      (unbox saved))))

;; a big array could generate 5 kW, or 5e6 mWh per hour.
(define ELEC-GEN-CEILING
  (let* ([kW 5]
         [mW (* kW 1000000)]
         [mWh-per-h mW]
         [mWh-per-m (/ mWh-per-h 60)]
         [mWh-per-15s (/ mWh-per-m 4)])
    (round mWh-per-15s)))

;; in tenths of a percent
(define INITIAL-HUMIDITY 664)


;; the ids of the temperature sensors

(define temperature-ids
  ;; all but the testing...
  (remove*
   (some-devices #px"^s-temp-testing-")
   (some-devices #px"^s-temp-")))

(define humidity-ids
  ;; turning off the kitchen for now...
  (some-devices #px"^s-hum-"))

;; generate a reading every so many seconds
(define TEMP-READING-SECONDS 60)

(define electrical-use-ids
  (some-devices #px"s-elec-used-"))

(define electrical-generation-ids
  (some-devices #px"s-elec-gen-"))

;; generate a reading every so many seconds
(define ELEC-READING-SECONDS 15)

(define (run-mock-temp-hum-elec)
  ;; start temperature threads:
  (for ([id (in-list temperature-ids)])
    (run-mock-device id TEMP-READING-SECONDS (make-temperature-generator)))
  
  ;; start humidity threads:
  (for ([id (in-list humidity-ids)])
    (run-mock-device id TEMP-READING-SECONDS (make-humidity-generator)))

  ;; start electrical use threads:
  (for ([id (in-list electrical-use-ids)])
    (let ([init (fetch-reading id)])
      (run-mock-device id ELEC-READING-SECONDS (make-electrical-use-generator init))))

  ;; start electrical generation threads
  (for ([id (in-list electrical-generation-ids)])
    (let ([init (fetch-reading id)])
      (run-mock-device id ELEC-READING-SECONDS (make-electrical-generation-generator init)))))





