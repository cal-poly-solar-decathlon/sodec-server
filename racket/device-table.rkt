#lang racket/base

(require racket/runtime-path)

(define-runtime-path here ".")

(provide id-lookup-table)

(define id-lookup-table
  (make-immutable-hash
   (with-input-from-file (build-path here "device-table.rktd")
     read)))

(module+ test
  (require rackunit)

  (check-equal? (hash-ref id-lookup-table "s-temp-lr")
                '("temperature" "living_room")))