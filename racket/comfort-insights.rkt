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

;; generate "comfort"-related insights

(define temperature-devices
  (filter
   (λ(str) (not (regexp-match #px"^testing" str)))
    (measurement-devices "temperature")))

(define indoor-temp-devices
  (remove "outside" temperature-devices))

(define humidity-devices
  (filter
   (λ (str) (not (regexp-match #px"^testing" str)))
   (measurement-devices "humidity")))

(define indoor-humidity-devices
  (remove "outside" humidity-devices))

(define COMFORT-MIN-TEMP 21.7)
(define COMFORT-MAX-TEMP 24.4)
;; multiplier that goes to maximum freak-out at 2.3 degrees C outside
;; of comfort zone
(define COMFORT-TEMP-RAMP (/ 25 2.3))

(define COMFORT-MAX-HUM 60)
;; maximum freak-out at 10% above max
(define COMFORT-HUM-RAMP (/ 25 10))

;; meanings of panic levels
;0-50 possibly interesting insights
;50-75 you should do this thing
;75-100 you are losing contest points right now!

(define FREAK-OUT-LEVEL 75)



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
    (for*/list ([temp-dev (in-list indoor-temp-devices)]
               [reading (in-value (device-latest-reading "temperature" temp-dev))]
               #:when reading)
      (/ reading 10)))
  (define outdoor-temp-reading
    (device-latest-reading "temperature" "outside"))
  (define outdoor-temp (and outdoor-temp-reading (/ outdoor-temp-reading 10)))
  (cond [(not (null? indoor-temps))
         (define indoor-temp-mean (mean indoor-temps))
         (define indoor-temp-stddev (stddev indoor-temps))
         (define cooler-outside? (and outdoor-temp (< outdoor-temp indoor-temp-mean)))
         (define warmer-outside? (and outdoor-temp (< indoor-temp-mean outdoor-temp)))
     
         (list ;; MEAN INTERIOR TEMPERATURE
          (cond [(< indoor-temp-mean COMFORT-MIN-TEMP)
                 (define urgency
                   (+ FREAK-OUT-LEVEL
                      (* COMFORT-TEMP-RAMP (- COMFORT-MIN-TEMP indoor-temp-mean))))
                 (cond [warmer-outside?
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a°C, "
                                          "which is below the contest minimum, "
                                          "but it's warmer outside. Open the windows.")
                                         (num-format indoor-temp-mean))
                                 urgency)]
                       [else
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a°C, "
                                          "which is below the contest minimum.")
                                         (num-format indoor-temp-mean))
                                 urgency)])]
                [(< indoor-temp-mean COMFORT-MAX-TEMP)
                 ;; temperature is okay
                 (cond [(and (< COMFORT-MIN-TEMP indoor-temp-mean)
                             (< outdoor-temp indoor-temp-mean))
                        (insight (format
                                  (string-append
                                   "The mean indoor temperature is ~a°C. You "
                                   "could cool the house by opening windows.")
                                  (num-format indoor-temp-mean))
                                 60)]
                       [(and (< indoor-temp-mean COMFORT-MAX-TEMP)
                             (< indoor-temp-mean outdoor-temp))
                        (insight (format
                                  (string-append
                                   "The mean indoor temperature is ~a°C. "
                                   "You could heat the house by opening windows")
                                  (num-format indoor-temp-mean))
                                 60)]
                       [else
                        (insight (format "The mean indoor temperature is ~a°C"
                                         (num-format indoor-temp-mean))
                                 25)])
                 ]
                [else
                 (define urgency
                   (+ FREAK-OUT-LEVEL
                      (* COMFORT-TEMP-RAMP (- indoor-temp-mean COMFORT-MAX-TEMP))))
                 (cond [cooler-outside?
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a°C, "
                                         "which is above the contest maximum, "
                                         "but it's cooler outside. Open the windows.")
                                         (num-format indoor-temp-mean))
                                 urgency)]
                       [else
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a°C, "
                                          "which is above the contest maximum.")
                                         (num-format indoor-temp-mean))
                                 urgency)])])
          ;; STDDEV OF INTERIOR TEMPS
          (insight (format "The standard deviation of indoor temperatures is ~a°C"
                           (num-format indoor-temp-stddev))
                   (min 100 (* indoor-temp-stddev 20))))]
        [else (list)]
  ))

(define (humidity-insights)
  (define indoor-humidities
    (for*/list ([humidity-dev (in-list indoor-humidity-devices)]
                [reading (in-value
                          (device-latest-reading "humidity" humidity-dev))]
                #:when reading)
      (/ reading 10)))
  (cond
    [(not (null? indoor-humidities))
     (define max-indoor-humidity
       (apply max indoor-humidities))
     (list
      (cond
        [(< max-indoor-humidity COMFORT-MAX-HUM)
         (insight (format "The maximum indoor humidity is ~a%"
                          (num-format max-indoor-humidity))
                  25)]
        [else (insight (format "The maximum indoor humidity is ~a%, which is higher than the contest maximum."
                               (num-format max-indoor-humidity))
                       (min 100
                            (+ FREAK-OUT-LEVEL
                               (* COMFORT-HUM-RAMP
                                  (- max-indoor-humidity COMFORT-MAX-HUM)))))]))]
        [else (list)]
  ))

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
        'p (exact->inexact (insight-priority insight))))

(module+ test
  (require rackunit)
  
  (check-equal? (insight->jsexpr
                 (insight "I'm so awesome!" 75.223))
                (hash 'm "I'm so awesome!"
                      'p 75.223))
  (comfort-insights)

  )