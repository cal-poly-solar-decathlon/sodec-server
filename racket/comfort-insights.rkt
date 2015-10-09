#lang racket/base

(require racket/contract
         "data-model.rkt"
         "device-table.rkt"
         (only-in math/statistics mean stddev)
         json)

(provide
 (contract-out
  [struct insight ([message string?]
                   [priority number?])]
  [comfort-insights (-> (listof insight?))]
  [insight->jsexpr (-> insight? jsexpr?)]))

;; convert a celsius temperature to a fahrenheit one
(define (c2f n)
  (+ 32 (* 9/5 n)))
;; convert a celsius *difference* in temperatures to a fahrenheit one
(define (dc2df n)
  (* 9/5 n))

;; generate "comfort"-related insights

(define temperature-devices
  (hash-ref measurement-device-table "temperature"))

(define indoor-temp-devices
  (filter (λ (d) (not (or (equal? d "outside")
                          (regexp-match #px"^testing_" d))))
          temperature-devices))

(define humidity-devices
  (hash-ref measurement-device-table "humidity"))

(define indoor-humidity-devices
  (filter (λ (d) (not (or (equal? d "outside")
                          (regexp-match #px"^testing_" d))))
          humidity-devices))

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

;; format a temperature
(define (temp-format n)
  (string-append (num-format (c2f n)) "°F"))

;; format a *difference* in temperatures
(define (dtemp-format n)
  (string-append (num-format (* 9/5 n)) "°F"))


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
  (temperatures->insights indoor-temps outdoor-temp))

;; given indoor temperatures and outdoor temperature in °C, formulate
;; insights
(define (temperatures->insights indoor-temps outdoor-temp)
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
                                          "The mean indoor temperature is ~a, "
                                          "which is below the contest minimum, "
                                          "but it's warmer outside. Open the windows.")
                                         (temp-format indoor-temp-mean))
                                 urgency)]
                       [else
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a, "
                                          "which is below the contest minimum.")
                                         (temp-format indoor-temp-mean))
                                 urgency)])]
                [(< indoor-temp-mean COMFORT-MAX-TEMP)
                 ;; temperature is okay
                 (cond [(and (< COMFORT-MIN-TEMP indoor-temp-mean)
                             (< outdoor-temp indoor-temp-mean))
                        (insight (format
                                  (string-append
                                   "The mean indoor temperature is ~a. You "
                                   "could cool the house by opening windows.")
                                  (temp-format indoor-temp-mean))
                                 60)]
                       [(and (< indoor-temp-mean COMFORT-MAX-TEMP)
                             (< indoor-temp-mean outdoor-temp))
                        (insight (format
                                  (string-append
                                   "The mean indoor temperature is ~a. "
                                   "You could heat the house by opening windows.")
                                  (temp-format indoor-temp-mean))
                                 60)]
                       ;; only happens when indoor mean EXACTLY EQUALS outdoor
                       [else
                        (insight (format "The mean indoor temperature is ~a."
                                         (temp-format indoor-temp-mean))
                                 25)])
                 ]
                [else
                 (define urgency
                   (+ FREAK-OUT-LEVEL
                      (* COMFORT-TEMP-RAMP (- indoor-temp-mean COMFORT-MAX-TEMP))))
                 (cond [cooler-outside?
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a, "
                                          "which is above the contest maximum, "
                                          "but it's cooler outside. Open the windows.")
                                         (temp-format indoor-temp-mean))
                                 urgency)]
                       [else
                        (insight (format (string-append
                                          "The mean indoor temperature is ~a, "
                                          "which is above the contest maximum.")
                                         (temp-format indoor-temp-mean))
                                 urgency)])])
          ;; STDDEV OF INTERIOR TEMPS
          (insight (format "The standard deviation of indoor temperatures is ~a"
                           (dtemp-format indoor-temp-stddev))
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
  (define outdoor-humidity-reading
    (device-latest-reading "humidity" "outside"))
  (define outdoor-humidity (and outdoor-humidity-reading (/ outdoor-humidity-reading 10)))
  (humidities->insights indoor-humidities outdoor-humidity))

;; given indoor humidities and outdoor humidity (or #f), return
;; insights
(define (humidities->insights indoor-humidities outdoor-humidity)
  (cond
    [(not (null? indoor-humidities))
     (define mean-indoor-humidity
       (mean indoor-humidities))
     (list
      (cond
        [(< mean-indoor-humidity COMFORT-MAX-HUM)
         (insight (format "The mean indoor humidity is ~a%."
                          (num-format mean-indoor-humidity))
                  25)]
        [else (define priority
                (min 100 (+ FREAK-OUT-LEVEL
                            (* COMFORT-HUM-RAMP
                               (- mean-indoor-humidity COMFORT-MAX-HUM)))))
              (cond [(and outdoor-humidity (< outdoor-humidity mean-indoor-humidity))
                     (insight (format "The mean indoor humidity is ~a%, which \
is higher than the contest maximum, but the outdoor humidity is lower. Open the windows."
                                      (num-format mean-indoor-humidity))
                              priority)]
                    [else
                     (insight (format "The mean indoor humidity is ~a%, which \
is higher than the contest maximum."
                               (num-format mean-indoor-humidity))
                              priority)])]))]
        [else (list)]))

(module+ test
)


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

  ;; LAZY MAN'S CHECKS!
  (check-not-exn (λ() (temperatures->insights null 60)))
  (check-not-exn (λ() (temperatures->insights '(12 19) 60)))
  (check-not-exn (λ() (temperatures->insights '(12 19) 5)))
  (check-not-exn (λ() (temperatures->insights '(22 24) 60)))
  (check-not-exn (λ() (temperatures->insights '(30 24) 60)))
  (check-not-exn (λ() (temperatures->insights '(30 24) 10)))
  (check-not-exn (λ() (temperatures->insights '(23 24) 10)))
  (check-not-exn (λ() (temperatures->insights '(23 24) 23.5)))
  

    (check-equal? (humidities->insights null 600) null)
  (check-equal? (humidities->insights '(24 34) 40)
                (list
                 (insight "The mean indoor humidity is 29.0%."
                         25)))
  (check-equal? (humidities->insights '(64 74) 40)
                (list
                 (insight
                  "The mean indoor humidity is 69.0%, which is higher than the contest \
maximum, but the outdoor humidity is lower. Open the windows."
                  195/2)))
  (check-equal? (humidities->insights '(64 74) 80)
                (list
                 (insight
                  "The mean indoor humidity is 69.0%, which is higher than the contest \
maximum."
                  195/2)))
  (check-equal? (humidities->insights '(64 74) #f)
                (list
                 (insight
                  "The mean indoor humidity is 69.0%, which is higher than the contest \
maximum."
                  195/2)))
  )