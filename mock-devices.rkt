#lang racket/base

(require "send-reading.rkt"
         "person-model.rkt")

(target-hosts '("localhost:8080"))

;; the lights:
(run-alice-barry-lights)

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
                (+ (unbox t-box)
                   (- (random 5) 2)))
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

(define temperature-ids
  '("s-temp-bed"
    "s-temp-bath"
    "s-temp-lr"
    "s-temp-out"))

;; start temperature threads:
(for ([id (in-list temperature-ids)])
  (run-mock-device id 5 (make-temperature-generator)))

(define humidity-ids
  '("s-hum-bed"
    "s-hum-bath"
    "s-hum-lr"
    "s-hum-out"))

;; start humidity threads:
(for ([id (in-list humidity-ids)])
  (run-mock-device id 5 (make-humidity-generator)))


;; don't die, just run forever...
(let loop ()
  (sleep 60)
  (loop))



