#lang racket

(require "person-model.rkt"
         "mock-devices.rkt"
         "send-reading.rkt")

(target-hosts '("localhost:8080" "localhost:3000"))


;; the lights:
(run-alice-barry-lights)
