#lang racket

;; one-off for extracting readings from the tail of a sql dump

(require "../device-table.rkt"
         "../data-model.rkt")

(module* test racket/base)

#;((define scraped-lines
  (file->lines "/tmp/gg.txt"))

scraped-lines 
(for/list ([l scraped-lines])
  (match
      (regexp-match #px"^\\([[:digit:]]+,'([^']+)','[^']+',([[:digit:]]+)\\)$" l)
    [(list dc id reading)
     (append (hash-ref id-lookup-table id) (string->number reading))
     ])))

;; freezing data:
(define old-readings
  '((("electricity_generated" "main_solar_array") 6048917081)
  (("electricity_used" "thermal_loop_pump") 1596859536)
  (("electricity_used" "dining_room_outlets_1") 1598799385)
  (("electricity_used" "water_supply_booster_pump") 1596411558)
  (("electricity_used" "bathroom_outlets") 1595070159)
  (("electricity_used" "heat_pump") 1598112686)
  (("electricity_used" "bedroom_outlets_2") 1595577729)
  (("electricity_used" "greywater_pump") 1597789614)
  (("electricity_used" "laundry") 1594856325)
  (("electricity_generated" "bifacial_solar_array") 6047130411)
  (("electricity_used" "entry_hall_outlets") 1597592736)
  (("electricity_used" "refrigerator") 1598738622)
  (("electricity_used" "water_heater") 1595445667)
  (("electricity_used" "living_room_outlets") 1595679173)
  (("electricity_used" "kitchen_outlets_2") 1596998629)
  (("electricity_used" "air_handler") 1598262651)
  (("electricity_used" "exterior_outlets") 1595174949)
  (("electricity_used" "vehicle_charging") 1597147708)
  (("electricity_used" "dining_room_outlets_2") 1594878710)
  (("electricity_used" "dishwasher") 1596870970)
  (("electricity_used" "water_supply_pump") 1597009491)
  (("electricity_used" "induction_stove") 1596310065)
  (("electricity_used" "blackwater_pump") 1598019749)
  (("electricity_used" "bedroom_outlets_1") 1597842330)
  (("electricity_used" "mechanical_room_outlets") 1596958556)
  (("electricity_used" "kitchen_outlets_1") 1595741711)))

(for ([reading old-readings])
  (record-device-status! (caar reading) (cadar reading) (cadr reading)))