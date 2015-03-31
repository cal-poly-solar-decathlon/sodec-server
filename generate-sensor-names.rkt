#lang racket

(define rooms'(("lr" "living room")
               ("bath" "bathroom")
               ("kit" "kitchen")
               ("bed" "bedroom")
               ("mech" "mechanical room")
               ("out" "outside north wall")))

;; generate the list of sensor names. Used to populate the
;; database.  This list must also match the one in ids.rkt

(provide sensor-names)

;- `hum` : Humidity : out, bed, bath, lr
;- `amb` : Ambient Light: bed, mech, lr, bath
;- `occ` : Occupancy sensors: bed, mech, lr, bath


(define sensor-patterns
  '(("s-temp" ("out" "bed" "bath" "lr") "the temperature in the ~a")
    ("s-hum" ("out" "bed" "bath" "lr") "the humidity in the ~a")
    ("s-occ" ("bed" "mech" "lr" "bath") "whether the ~a is occupied")
    ("s-amb" ("bed" "mech" "lr" "bath") "the ambient light level in the ~a")
    #;("c-light" ("bed" "mech" "lr" "bath" "kit") "control of the lights in the ~a")
    #;("s-light" "a record of the lighting control events in the ~a")))

(define light-spec
  "Entry Bookend (1A)
Chandelier (1B)
TV Light (2A)
Kitchen Uplight (3A)
Under-Counter (3B)
Pendant Bar Lights (3C)
Bathroom Ambient (4A)
Mirror (4B)
Flexspace Uplight (5A)
Flexspace Cabinet (5B)
Bedroom Uplight (6A)
Bedroom Cabinet (6B)
Porch Lights (8A)
Uplights and Pot Lights (8B)")


(define extra-devices
  '(("s-temp-testing-blackhole" "a temperature bin to test recording")
    ("s-temp-testing-empty" "a temperature bin that's always empty")))

#;()

(for-each
 display
(append
 (for*/list ([pat (in-list sensor-patterns)]
            [r (in-list (second pat))])
  (~a "- `"(first pat)"-"r"` : "(format (third pat) (second (assoc r rooms)))"\n"))
 (for/list ([d (in-list extra-devices)])
   (~a "- `"(first d)"` : "(second d)"\n"))))

;; as strings, for ids.rkt:
(define device-strs
  (append
   (for*/list ([pat (in-list sensor-patterns)]
               [r (in-list (second pat))])
     (~a (first pat)"-"r))
   (for/list ([d extra-devices])
     (first d))))

(define sensor-names
  (map string->symbol device-strs))





