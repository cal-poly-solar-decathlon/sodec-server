#lang racket/base

(require "send-reading.rkt"
         "person-model.rkt")

(target-hosts '("calpolysolardecathlon.org:8080"))

;; the lights:
(run-alice-barry-lights)
