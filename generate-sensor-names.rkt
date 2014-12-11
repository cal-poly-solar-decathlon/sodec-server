#lang racket

(define rooms'(("lr" "living room")
               ("bath" "bathroom")
               ("kit" "kitchen")
               ("bed" "bedroom")))
(define sensor-patterns
  '(("s-temp" "the temperature in the ~a")
    ("s-occ" "whether the ~a is occupied")
    ("s-amb" "the ambient light level in the ~a")
    ("c-light" "control of the lights in the ~a")
    ("s-light" "a record of the lighting control events in the ~a")))

(for-each
 display
(for*/list ([r (in-list rooms)]
            [pat (in-list sensor-patterns)])
  (~a "- `"(first pat)"-"(first r)"` : "(format (second pat) (second r))"\n")))

(for*/list ([r (in-list rooms)]
            [pat (in-list sensor-patterns)])
  (~a (first pat)"-"(first r)))

