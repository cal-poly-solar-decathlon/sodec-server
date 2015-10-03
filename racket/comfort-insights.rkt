#lang racket/base

(require racket/contract
         "data-model.rkt"
         (only-in math/statistics mean stddev)
         json)

(provide
 (contract-out
  [struct insight ([message string?]
                   [priority number?])]
  [generate-comfort-insights (-> (listof insight?))]
  [insight->jsexpr (-> insight? jsexpr?)]))


;; generate "comfort"-related insights

(define temperature-devices
  (filter
   (λ(str) (not (regexp-match #px"^testing" str)))
    (measurement-devices "temperature")))

(define inside-temp-devices
  (remove "outside" temperature-devices))

(define humidity-devices
  (filter
   (λ (str) (not (regexp-match #px"^testing" str)))
   (measurement-devices "humidity")))

(define inside-humidity-devices
  (remove "outside" humidity-devices))

(define COMFORT-MIN-TEMP 21.7)
(define COMFORT-MAX-TEMP 24.4)
;; multiplier that goes to maximum freak-out at 2.3 degrees C outside
;; of comfort zone
(define COMFORT-TEMP-RAMP (/ 100 2.3))

(define COMFORT-MAX-HUM 60)
;; maximum freak-out at 10% above max
(define COMFORT-HUM-RAMP (/ 100 10))

(define (num-format n)
  (number->string (exact->inexact (/ (round (* 10 n)) 10))))


;; an insight contains a string and a "priority" from 0 to 100
;; indicating how important it is. These priorities are used to rank
;; the insights
(struct insight (message priority) #:transparent)

(define (generate-comfort-insights)
  (define inside-temps
    (for/list ([temp-dev (in-list inside-temp-devices)])
      (/ (device-latest-reading "temperature" temp-dev) 10)))
  (define temp-mean (mean inside-temps))
  (define temp-stddev (stddev inside-temps))
  (define inside-humidities
    (for/list ([humidity-dev (in-list inside-humidity-devices)])
      (/ (device-latest-reading "humidity" humidity-dev) 10)))
  (define max-inside-humidity
    (apply max inside-humidities))
  (join-insights
   ;; MEAN INTERIOR TEMPERATURE
   (cond [(< temp-mean COMFORT-MIN-TEMP)
          (insight "The mean inside temperature is ~a°C, which is below the contest minimum!"
          (* COMFORT-TEMP-RAMP (- COMFORT-MIN-TEMP temp-mean)))]
         [(< temp-mean COMFORT-MAX-TEMP)
          (insight "The mean inside temperature is ~a°C"
          0)]
         [else
          (insight (format "The mean inside temperature is ~a°C, which is above the contest minimum!"
                           (num-format temp-mean))
                   (* COMFORT-TEMP-RAMP (- temp-mean COMFORT-MAX-TEMP)))])
   ;; STDDEV OF INTERIOR TEMPS
   (insight (format "The standard deviation of interior temperatures is ~a°C"
                    (num-format temp-stddev))
            (min 100 (* temp-stddev 20)))
   ;; INTERIOR HUMIDITY
   (insight (format "The maximum interior humidity is ~a%"
                    (num-format max-inside-humidity))
            (cond [(< max-inside-humidity COMFORT-MAX-HUM)
                   0]
                  [else (min 100
                             (* COMFORT-HUM-RAMP
                                (- max-inside-humidity COMFORT-MAX-HUM)))]))
   )
  )

(define (join-insights . args)
  (sort args > #:key insight-priority))

(define (insight->jsexpr insight)
  (hash 'm (insight-message insight)
        'p (insight-priority insight)))

(module+ test
  (require rackunit)
  
  (check-equal? (insight->jsexpr
                 (insight "I'm so awesome!" 75.223))
                (hash 'm "I'm so awesome!"
                      'p 75.223))
  (generate-comfort-insights)

  )