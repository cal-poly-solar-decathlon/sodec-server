#lang typed/racket/base

(require typed/racket/date
         "data-model.rkt")

(provide record-temperature!)

;; a temp is an integer, representing 
;; thousandths of a degree Celsius

(define TEMPERATURE-ID "s-temp-kit")
(define TEMP-ROUNDING 1/1000)
(define INITIAL-TEMPERATURE (/ 20 TEMP-ROUNDING))

(define temperature-box (box INITIAL-TEMPERATURE))


;; record the temperature
(: record-temperature! (Natural -> Void))
(define (record-temperature! temp)
  (record-sensor-status! TEMPERATURE-ID (number->string temp)))

;; BOGUS TEMPERATURE READING THREAD
;; adds a new reading every five seconds
(thread 
 (lambda ()
   (let loop ()
     (set-box! temperature-box
               (+ (unbox temperature-box)
                  (- (random 500) 250)))
     (record-temperature! (unbox temperature-box))
     (sleep 5)
     (loop))))


