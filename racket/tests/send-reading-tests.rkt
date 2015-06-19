#lang racket

(require rackunit
         "../send-reading.rkt")


(define the-test-suite
  (test-suite
   "send-reading"

   (parameterize ([target-hosts '("calpolysolardecathlon.org:8080")])
     (check-not-exn
      (lambda () (send-reading! "s-temp-testing-blackhole" 334))))

   (check-equal?
    (send-reading!/core "calpolysolardecathlon.org:8080" "s-temp-testing-blackhole" 334)
    "okay")

   (check-match
    (call-with-values (lambda () (time-apply send-reading!/core
                                             (list
                                              "calpolysolardecathlon.org:8080"
                                              "s-temp-testing-blackhole" 334)))
                      list)
    (list (list "okay") b (? (lambda (t) (< t 1000)) _) d))))

(module+ test
  (require rackunit/text-ui)

  
  (run-tests the-test-suite))