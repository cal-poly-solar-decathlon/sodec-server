#lang racket/base

(require racket/runtime-path
         racket/block
         racket/contract)

(define-runtime-path here ".")

(provide
 (contract-out
  [id-lookup-table
   (hash/c string? (list/c measurement? string?)
           #:immutable #t)]
  [MEASUREMENT-NAMES (listof string?)]
  [measurement-device-table
   (hash/c measurement? (listof string?))]))

(define (measurement? n)
  (member n MEASUREMENT-NAMES))

(define MEASUREMENT-NAMES
  '("temperature"
    "humidity"
    "electric_power"
    ;; add others as necessary...
    ))

(define table-data
  (with-input-from-file (build-path here "device-table.rktd")
    read))

;; To add a new one: all that's really necessary is to add it
;; to the eGauge configuration and to the web page; the server
;; will just cheerfully record whatever it gets. It only appears
;; here for the purpose of mock data generation and testing.
(define electric-power-devices
  '("laundry"
    "dishwasher"
    "refrigerator"
    "induction_stove"
    "water_heater"
    "kitchen_outlets_1"
    "kitchen_outlets_2"
    "living_room_outlets"
    "dining_room_outlets_1"
    "dining_room_outlets_2"
    "bathroom_outlets"
    "bedroom_outlets_1"
    "bedroom_outlets_2"
    "mechanical_room_outlets"
    "entry_hall_outlets"
    "exterior_outlets"
    "greywater_pump"
    "blackwater_pump"
    "thermal_loop_pump"
    "water_supply_pump"
    "water_supply_booster_pump"
    "vehicle_charging"
    "heat_pump"
    "air_handler"
    "air_conditioning"
    "microwave"
    "lighting_1"
    "lighting_2"
    "mains"))

;; this table maps old ids to measurement/device lists
;; NB: TEMPERATURE AND HUMIDITY ONLY
(define id-lookup-table
  (make-immutable-hash
   table-data))

;; this table maps measurements to the legal devices
(define measurement-device-table
  (block
   ;; first, add the temp/hum devices:
   (define table-a
     (for/fold ([ht (hash)])
               ([pr (map cdr table-data)])
       (hash-set ht (car pr) (cons (cadr pr) (hash-ref ht (car pr) null)))))
   (hash-set table-a "electric_power" electric-power-devices)))


(module+ test
  (require rackunit)

  (check-equal? (hash-ref id-lookup-table "s-temp-lr")
                '("temperature" "living_room"))

  (check-match (hash-ref measurement-device-table "temperature")
               (list-no-order "testing_empty" (? string? _) ...))

  ;; Electric Power no longer appears in the device table. It's
  ;; just dynamic; whatever shows up, we stow it in the database.
  #;(check-equal? (hash-ref id-lookup-table "s-elec-used-air-handler-recep")
                '("electricity_used" "air_handler")))