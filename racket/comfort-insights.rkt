#lang racket/base

(require racket/contract
         "data-model.rkt"
         (only-in math/statistics mean stddev)
         json)

(provide
 (contract-out
  [struct insight ([message string?]
                   [priority number?])]
  [comfort-insights (-> (listof insight?))]
  [insight->jsexpr (-> insight? jsexpr?)]))

;; meanings of panic levels
;0-25 possibly interesting insights
;25-50 you should do this thing
;50-100 you are losing contest points right now!


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

(define (comfort-insights)
  (join-insights
   (append
    (temperature-insights)
    (humidity-insights)
    (forecast-insights))))

(define (temperature-insights)
  (define indoor-temps
    (for*/list ([temp-dev (in-list inside-temp-devices)]
               [reading (in-value (device-latest-reading "temperature" temp-dev))]
               #:when reading)
      (/ reading 10)))
  (cond [(not (null? indoor-temps))
         (define indoor-temp-mean (mean indoor-temps))
         (define indoor-temp-stddev (stddev indoor-temps))
         (join-insights
          (append
           (maybe-open-windows-insights indoor-temp-mean )
           (list ;; MEAN INTERIOR TEMPERATURE
            (cond [(< indoor-temp-mean COMFORT-MIN-TEMP)
                   (insight (format "The mean inside temperature is ~a°C, which is below the contest minimum."
                                    (num-format indoor-temp-mean))
                            (* COMFORT-TEMP-RAMP (- COMFORT-MIN-TEMP indoor-temp-mean)))]
                  [(< indoor-temp-mean COMFORT-MAX-TEMP)
                   (insight "The mean inside temperature is ~a°C"
                            0)]
                  [else
                   (insight (format "The mean inside temperature is ~a°C, which is above the contest minimum."
                                    (num-format indoor-temp-mean))
                            (* COMFORT-TEMP-RAMP (- indoor-temp-mean COMFORT-MAX-TEMP)))])
            ;; STDDEV OF INTERIOR TEMPS
            (insight (format "The standard deviation of interior temperatures is ~a°C"
                             (num-format indoor-temp-stddev))
                     (min 100 (* indoor-temp-stddev 20)))) ) )]
        [else (list)]
  ))

(define (maybe-open-windows-insights indoor-mean-temp outdoor-temp)
  (list)
  #;(list 
   (cond [(and (< COMFORT-MIN-TEMP indoor-mean-temp)
               (< outdoor-temp indoor-mean-temp))
          (insight "You could cool the house by opening windows"
                   )]
         [(and (< indoor-mean-temp COMFORT-MAX-TEMP)
               (< indoor-mean-temp outdoor-temp))])))

(define (humidity-insights)
  (define inside-humidities
    (for/list ([humidity-dev (in-list inside-humidity-devices)])
      (/ (device-latest-reading "humidity" humidity-dev) 10)))
  (define max-inside-humidity
    (apply max inside-humidities))
  (cond [(< max-inside-humidity COMFORT-MAX-HUM)
         (insight (format "The maximum interior humidity is ~a%"
                          (num-format max-inside-humidity))
                  0)]
        [else (insight (format "The maximum interior humidity is ~a%, which is higher than the contest maximum."
                               (num-format max-inside-humidity))
                       (min 100
                            (* COMFORT-HUM-RAMP
                               (- max-inside-humidity COMFORT-MAX-HUM))))]))

;; basic outdoor-temperature insight
(define (indoor-outdoor-insights)
  (list))

;; insights based on the forecast
(define (forecast-insights)
  (list)
  )


(define (join-insights insights)
  (sort insights > #:key insight-priority))

(define (insight->jsexpr insight)
  (hash 'm (insight-message insight)
        'p (insight-priority insight)))

(module+ test
  (require rackunit)
  
  (check-equal? (insight->jsexpr
                 (insight "I'm so awesome!" 75.223))
                (hash 'm "I'm so awesome!"
                      'p 75.223))
  (comfort-insights)

  )