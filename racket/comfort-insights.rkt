#lang racket/base

(require racket/contract
         "data-model.rkt"
         "device-table.rkt"
         "insight-struct.rkt"
         (only-in math/statistics mean stddev)
         json)

(provide
 (contract-out
  [comfort-insights (-> (listof insight?))]))



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





(define (comfort-insights)
  (join-insights
   (append
    (temperature-insights)
    (humidity-insights))))

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
  (require rackunit)
  
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