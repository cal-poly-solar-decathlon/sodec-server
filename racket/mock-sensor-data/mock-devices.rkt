#lang racket/base

(require "device-readings.rkt"
         "../device-table.rkt")

(provide run-mock-temp-hum-elec)

;; the temperature and humidity:

(define TEMP-QUANTA 10)
(define TEMP-ROUNDING (/ 1 TEMP-QUANTA))
(define INITIAL-TEMPERATURE (* 20 TEMP-QUANTA))

;; generate a reading every so many seconds
(define ELEC-READING-SECONDS 15)
(define TEMP-READING-SECONDS 60)


;; generate a mock device reading every 'interval' seconds,
;; using a given generator
(define (run-mock-device measurement id interval generator)
  (thread 
   (lambda ()
     (let loop ()
       (thread (lambda () (send-reading! measurement id (generator))))
       (sleep interval)
       (loop)))))

;; a temperature generator
(define (make-temperature-generator)
  (let ([t-box (box INITIAL-TEMPERATURE)])
    (lambda ()
      (set-box! t-box
                (max 0
                     (+ (unbox t-box)
                        (- (random 5) 2))))
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

;; an electrical use generator. only goes up.

(define (make-electrical-use-generator init)
  (let ([saved (box init)])
    (lambda ()
      (set-box! saved
                (+ (unbox saved)
                   (random ELEC-USE-CEILING)))
      (unbox saved))))

;; a really big device would use 1500 watts
(define ELEC-USE-CEILING
  (let* ([W 1500]
         [Ws-per-s W]
         [Ws-per-interval (* Ws-per-s ELEC-READING-SECONDS)])
    (round Ws-per-interval)))

;; an electrical generator generator. only goes up:

(define (make-electrical-generation-generator init)
  (let ([saved (box init)])
    (lambda ()
      (set-box! saved
                (+ (unbox saved)
                   (random ELEC-GEN-CEILING)))
      (unbox saved))))

;; a big array could generate 5 kW
(define ELEC-GEN-CEILING
  (let* ([kW 5]
         [W (* kW 1000)]
         [Ws-per-s W]
         [Ws-per-interval (* Ws-per-s ELEC-READING-SECONDS)])
    (round Ws-per-interval)))

;; in tenths of a percent
(define INITIAL-HUMIDITY 664)


;; the ids of the temperature devices

(define temperature-devices
  (filter (lambda (s)
            (not (regexp-match #px"^testing_" s)))
          (hash-ref measurement-device-table "temperature")))

(define humidity-devices
  (filter (lambda (s)
            (not (regexp-match #px"^testing_" s)))
          (hash-ref measurement-device-table "humidity")))

(define electric-use-devices
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
    "air_handler"))

(define electric-generation-devices
  '("main_solar_array"
    "bifacial_solar_array"))

(define (run-mock-temp-hum-elec)
  ;; start temperature threads:
  (for ([device (in-list temperature-devices)])
    (run-mock-device "temperature" device TEMP-READING-SECONDS
                     (make-temperature-generator)))
  
  ;; start humidity threads:
  (for ([device (in-list humidity-devices)])
    (run-mock-device "humidity" device TEMP-READING-SECONDS
                     (make-humidity-generator)))
  
  ;; start electrical use threads:
  (for ([device (in-list electric-use-devices)])
    (let ([init (fetch-reading "electric_power" device)])
      (run-mock-device "electric_power" device ELEC-READING-SECONDS
                       (make-electrical-use-generator init))))
  
  ;; start electrical generation threads
  (for ([device (in-list electric-generation-devices)])
    (let ([init (fetch-reading "electric_power" device)])
      (run-mock-device "electric_power" device ELEC-READING-SECONDS
                       (make-electrical-generation-generator init)))))





