#lang typed/racket/base

(require typed/racket/date
         "data-model.rkt"
         "time.rkt"
         "testing-param.rkt")

(provide record-temperature!
         handle-temperature-request)

;; a temp is an integer, representing 
;; thousandths of a degree Celsius

(define TEMPERATURE-ID 1)
(define TEMP-ROUNDING 1/1000)
(define INITIAL-TEMPERATURE (/ 20 TEMP-ROUNDING))

(define temperature-box (box INITIAL-TEMPERATURE))


;; record the temperature
(: record-temperature! (Natural -> Void))
(define (record-temperature! temp)
  (record-sensor-status! TEMPERATURE-ID (number->string temp)))

;; convert a temperature status to a jsexpr
(: temp-status->jsexpr (Status -> (HashTable Symbol Any)))
(define (temp-status->jsexpr status)
  (make-immutable-hash
   (list (cons 'timestamp 
               ;; there must be some standard JSON date format...
               (date->string (Status-timestamp status) #t))
         (cons 'reading (string->number (Status-status status))))))

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

;; respond to a temperature request
(define (handle-temperature-request)
  (map temp-status->jsexpr (sensor-statuses TEMPERATURE-ID)))


;; can't use submodules with typed?

(require typed/rackunit)

;; testing db created by data-model
(parameterize ([testing? #t])
  (check-equal? (handle-temperature-request)
                (list (make-immutable-hash
                       (list (cons 'timestamp 
                                   "Friday, January 1st, 1971 12:00:00am")
                             (cons 'reading 22900)))
                      (make-immutable-hash
                       (list (cons 'timestamp
                                   "Friday, January 1st, 1971 12:00:02am")
                             (cons 'reading 22883))))))

