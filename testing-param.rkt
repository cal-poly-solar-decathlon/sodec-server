#lang typed/racket/base

;; defines the testing parameter.

(provide testing?)

;; are we in testing mode (i.e., using testing tables)?
(: testing? (Parameterof Boolean))
(define testing? 
  (make-parameter #f 
                  (lambda (b)
                    (cond [(boolean? b) b]
                          [else 
                           (raise-argument-error
                            'testing?
                            "boolean"
                            0 b)]))))
