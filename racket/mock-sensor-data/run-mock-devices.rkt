#lang racket

(require "person-model.rkt"
         "mock-devices.rkt"
         "device-readings.rkt")

;; this is the loop that actually sends mock data to the existing servers

(target-hosts '(("localhost" 8080) #;("localhost" 3000)))


;; the lights:
(run-alice-barry-lights)

;; everything else
(run-mock-temp-hum-elec)


;; don't die, just run forever...
(let loop ()
  (sleep 60)
  (loop))