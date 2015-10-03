#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/block
         "../web-funs.rkt"
         "../data-model.rkt")

(run-tests
 (test-suite
  "data echo testing"
  
  (block
   (define ECHO-HOST "calpolysolardecathlon.org")
   (define ECHO-PORT 3000)
   (define MEASUREMENT "electric_power")
   (define DEVICE "testing_data_echo")
   (define READING 22739817)
   (test-case
    "data echo"
    (parameterize ([echo-data-to-host (list ECHO-HOST ECHO-PORT)])
      (record-device-status! MEASUREMENT DEVICE READING)
      ;; wait for echo to be finished...
      (sleep 3)
      (check-equal?
       (remote-call/get "calpolysolardecathlon.org" 3000
                        (sodec-url "latest-event"
                                   `((measurement ,MEASUREMENT)
                                     (device ,DEVICE))))
       READING))))))

