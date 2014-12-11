#lang typed/racket/base

(provide ID ID?)

;; PUT ALL OF THE LEGAL DEVICE IDS HERE:

(define-type ID
  (U "s-temp-lr"
     "s-occ-lr"
     "s-amb-lr"
     "c-light-lr"
     "s-light-lr"
     "s-temp-bath"
     "s-occ-bath"
     "s-amb-bath"
     "c-light-bath"
     "s-light-bath"
     "s-temp-kit"
     "s-occ-kit"
     "s-amb-kit"
     "c-light-kit"
     "s-light-kit"
     "s-temp-bed"
     "s-occ-bed"
     "s-amb-bed"
     "c-light-bed"
     "s-light-bed"))

(define-predicate ID? ID)