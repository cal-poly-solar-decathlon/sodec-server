#lang racket

(provide measurement-devices)

(define TEMP-HUM-LOCATIONS
  (map symbol->string
       '(living_room
         bedroom
         kitchen
         outside
         bathroom
         testing_blackhole
         testing_empty)))

(define measurement-devices
  `(("temperature"
     ,TEMP-HUM-LOCATIONS)
    ("humidity"
     ,TEMP-HUM-LOCATIONS)
    ("electricity_used"
     ())
    ("electricity_generated"
     ())
    ("light_level"
     ())
    ("occupied"
     ())))

