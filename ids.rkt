#lang typed/racket/base

(provide ID ID?)

;; PUT ALL OF THE LEGAL DEVICE IDS HERE:

(define-type ID
  (U "s-temp-lr"
     "s-temp-kit"))

(define-predicate ID? ID)