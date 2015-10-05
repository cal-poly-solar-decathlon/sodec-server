#lang racket/base

(require racket/date
         racket/contract)

(provide ts-seconds?
         seconds?
         ts-milliseconds?)

;; dates before the year 2000 or after 3000 are likely
;; to be unit errors:
(define MIN-REASONABLE-SECONDS
  (find-seconds 0 0 0 1 1 2000))
(define MAX-REASONABLE-SECONDS
  (find-seconds 0 0 0 1 1 2030))
(define MIN-REASONABLE-MILLISECONDS
  (* MIN-REASONABLE-SECONDS 1000))
(define MAX-REASONABLE-MILLISECONDS
  (* MAX-REASONABLE-SECONDS 1000))

;; a number of seconds used as a timestamp
(define ts-seconds? (integer-in MIN-REASONABLE-SECONDS MAX-REASONABLE-SECONDS))
;; a number of seconds
(define seconds? exact-integer?)
;; a number of milliseconds used as a timestamp
(define ts-milliseconds?
  (integer-in MIN-REASONABLE-MILLISECONDS MAX-REASONABLE-MILLISECONDS))