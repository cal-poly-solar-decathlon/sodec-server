#lang racket/base

(require "../device-table.rkt"
         "../data-model-influx.rkt")


;; certain measurements need to have at least one initial zero.
(define (ensure-at-least-one-reading measurement)
  (for ([device (in-list (hash-ref measurement-device-table measurement))])
    (when (not (sensor-latest-reading measurement device))
      (printf "inserting default zero value for cumulative device: ~v\n" device)
      (record-sensor-status! measurement device 0))))

(ensure-at-least-one-reading "electricity_used")
(ensure-at-least-one-reading "electricity_generated")
