#lang racket/base

(require "device-readings.rkt"
         "person-model.rkt")

;; this file is just for testing the person-model by itself

(target-hosts '("calpolysolardecathlon.org:8080"))

;; the lights:
(run-alice-barry-lights)
