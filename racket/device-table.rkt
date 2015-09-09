#lang racket/base

(require racket/runtime-path)

(define-runtime-path here ".")

(provide id-lookup-table
         measurement-device-table
         all-ids
         some-ids)

(define table-data
  (with-input-from-file (build-path here "device-table.rktd")
    read))

;; all of the old-style ids
(define all-ids
  (map car table-data))

;; all of the devices that match the given regexp
(define (some-ids regexp)
  (for/list ([device-name (in-list all-ids)]
             #:when (regexp-match regexp device-name))
    device-name))

;; this table maps old ids to measurement/device lists
(define id-lookup-table
  (make-immutable-hash
   table-data))

;; this table maps measurements to the legal devices
(define measurement-device-table
  (for/fold ([ht (hash)])
            ([pr (map cdr table-data)])
    (hash-set ht (car pr) (cons (cadr pr) (hash-ref ht (car pr) null)))))


(module+ test
  (require rackunit)

  (check-equal? (hash-ref id-lookup-table "s-temp-lr")
                '("temperature" "living_room"))

  (check-match (hash-ref measurement-device-table "temperature")
               (list-no-order "testing_empty" (? string? _) ...))

  (check-equal? (hash-ref id-lookup-table "s-elec-used-air-handler-recep")
                '("electricity_used" "air_handler")))