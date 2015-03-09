#lang racket

(define rooms'(("lr" "living room")
               ("bath" "bathroom")
               ("kit" "kitchen")
               ("bed" "bedroom")
               ("mech" "mechanical room")
               ("out" "outside north wall")))

;- `hum` : Humidity : out, bed, bath, lr
;- `amb` : Ambient Light: bed, mech, lr, bath
;- `occ` : Occupancy sensors: bed, mech, lr, bath


(define sensor-patterns
  '(("s-temp" ("out" "bed" "bath" "lr") "the temperature in the ~a")
    ("s-hum" ("out" "bed" "bath" "lr") "the humidity in the ~a")
    ("s-occ" ("bed" "mech" "lr" "bath") "whether the ~a is occupied")
    ("s-amb" ("bed" "mech" "lr" "bath") "the ambient light level in the ~a")
    ("c-light" ("bed" "mech" "lr" "bath" "kit") "control of the lights in the ~a")
    #;("s-light" "a record of the lighting control events in the ~a")))

(for-each
 display
(for*/list ([pat (in-list sensor-patterns)]
            [r (in-list (second pat))])
  (~a "- `"(first pat)"-"r"` : "(format (third pat) (second (assoc r rooms)))"\n")))

(for*/list ([r (in-list rooms)]
            [pat (in-list sensor-patterns)])
  (~a (first pat)"-"(first r)))

