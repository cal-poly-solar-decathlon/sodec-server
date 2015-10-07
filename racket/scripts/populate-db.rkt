#lang racket/base

(require "../device-table.rkt"
         "../data-model.rkt")


(module* test racket/base)

;; certain measurements need to have at least one initial zero.
(define (ensure-at-least-one-reading measurement)
  (for ([device (in-list (hash-ref measurement-device-table measurement))])
    (when (not (device-latest-reading measurement device))
      (printf "inserting default zero value for cumulative device: ~v\n" device)
      (record-device-status! measurement device 0))))

(ensure-at-least-one-reading "electricity_used")
(ensure-at-least-one-reading "electricity_generated")
