#lang racket/base

(require "device-readings.rkt"
         racket/match)

(provide run-mock-electric)

;; need a more sophisticated model, slightly.

;; a status is a map from independent devices to current readings

(define electric-use-devices
  '("laundry"
    "dishwasher"
    "refrigerator"
    "induction_stove"
    "water_heater"
    "greywater_pump"
    "blackwater_pump"
    "thermal_loop_pump"
    "water_supply_pump"
    "water_supply_booster_pump"
    "vehicle_charging"
    "mechanical_room_outlets"
    "heat_pump"
    "air_handler"
    "air_conditioning"
    "microwave"
    "lighting_1"
    "lighting_2"))

(define electric-generation-devices
  '("main_solar_array"
    "bifacial_solar_array"))

(define special-devices
  '("rest_of_house"
    "mains"))

;; electric readings happen this often:
(define ELEC-READING-SECONDS 15)

;; convert watts to watt-seconds-per-interval
(define (watts->ws-per-interval watts)
  (let* ([Ws-per-s watts]
         [Ws-per-interval (* Ws-per-s ELEC-READING-SECONDS)])
    (round Ws-per-interval)))

;; a really big device would use 1500 watts
(define ELEC-USE-CEILING (watts->ws-per-interval 1500))
;; the mains could use as much as 200 Amps * 120 Volts
(define ELEC-MAINS-CEILING (watts->ws-per-interval (* 200 120)))
;; a big array could generate 5 kW
(define ELEC-GEN-CEILING (watts->ws-per-interval 5000))

;; this table says for every device beside "mains"
;; what the ceiling of the generation is:
(define generation-table
  (append
   (for/list ([device (in-list electric-use-devices)])
     (list device (list 0 ELEC-USE-CEILING)))
   (for/list ([device (in-list electric-generation-devices)])
     (list device (list (- ELEC-GEN-CEILING) 0)))
   (list
    (list "rest_of_house" (list 0 (* 3 ELEC-USE-CEILING))))))

;; generate diffs for the values other than the mains:
(define (generate-not-mains-diffs)
  (make-immutable-hash
   (for/list ([entry (in-list generation-table)])
     (cons (car entry)
           (+ (caadr entry)
              (random (- (cadadr entry) (caadr entry))))))))

;; given old readings, generate new ones (incl. mains)
(define (update-readings old-readings)
  (define diffs (generate-not-mains-diffs))
  (define mains-diff (- (apply + (hash-values diffs))))
  (for/list ([pr (in-list old-readings)])
    (match pr
      [(list "mains" n)
       (list "mains" (+ n mains-diff))]
      [(list other-name n)
       (list other-name (+ n (hash-ref diffs other-name)))])))

;; the initial readings (we want to maintain continuity)
(define (fetch-base-readings)
  (for/list ([device (in-list (append
                               electric-use-devices
                               electric-generation-devices
                               (list "mains")))])
    (let ([init (fetch-reading "electric_power" device)])
      (list device init))))

;; send all of the readings
(define (send-readings readings)
  (for ([pr (in-list readings)])
    (thread (lambda () (send-reading! "electric_power"
                                      (car pr) (cadr pr))))))

(define (run-mock-electric)
  (define base-readings (fetch-base-readings))
  (thread
   (λ()
     (let loop ([readings base-readings])
       (define new-readings (update-readings readings))
       (send-readings new-readings)
       (sleep ELEC-READING-SECONDS)
       (loop new-readings)))))
