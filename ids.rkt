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
     "s-temp-testing-empty"
     "s-light-entry-bookend-1A"
     "s-light-chandelier-1B"
     "s-light-tv-light-2A"
     "s-light-kitchen-uplight-3A"
     "s-light-under-counter-3B"
     "s-light-pendant-bar-lights-3C"
     "s-light-bathroom-ambient-4A"
     "s-light-mirror-4B"
     "s-light-flexspace-uplight-5A"
     "s-light-flexspace-cabinet-5B"
     "s-light-bedroom-uplight-6A"
     "s-light-bedroom-cabinet-6B"
     "s-light-porch-lights-8A"
     "s-light-uplights-and-pot-lights-8B"))


(define-predicate ID? ID)