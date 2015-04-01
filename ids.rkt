#lang typed/racket/base

(provide ID ID?)

;; PUT ALL OF THE LEGAL DEVICE IDS HERE:

(define-type ID
  (U "s-temp-out"
     "s-temp-bed"
     "s-temp-bath"
     "s-temp-lr"
     "s-hum-out"
     "s-hum-bed"
     "s-hum-bath"
     "s-hum-lr"
     "s-occ-bed"
     "s-occ-mech"
     "s-occ-lr"
     "s-occ-bath"
     "s-amb-bed"
     "s-amb-mech"
     "s-amb-lr"
     "s-amb-bath"
     "s-temp-testing-blackhole"
     "s-temp-testing-empty"))

(define-predicate ID? ID)