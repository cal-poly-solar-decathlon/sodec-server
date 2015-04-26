#lang racket

(require "person-model.rkt"
         "mock-devices.rkt"
         "send-reading.rkt")

(target-hosts '("localhost:8080" "localhost:3000"))


;; the lights:
(run-alice-barry-lights)

;; everything else
(run-mock-temp-hum-elec)


;; don't die, just run forever...
(let loop ()
  (sleep 60)
  (loop))