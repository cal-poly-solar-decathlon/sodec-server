#lang racket/base

(require racket/contract
         json)

(provide
 (contract-out
  [FREAK-OUT-LEVEL number?]
  [SHOULD-ACT number?]
  [struct insight ([message string?]
                   [priority number?])]
  [insight->jsexpr (-> insight? jsexpr?)]
  [join-insights (-> (listof insight?) (listof insight?))]
  [num-format (-> number? string?)]
  [temp-format (-> number? string?)]
  [dtemp-format (-> number? string?)]))


;; an insight contains a string and a "priority" from 0 to 100
;; indicating how important it is. These priorities are used to rank
;; the insights
(struct insight (message priority) #:transparent)

;; meanings of panic levels
;0-50 possibly interesting insights
;50-75 you should do this thing
;75-100 you are losing contest points right now!
(define SHOULD-ACT 50)
(define FREAK-OUT-LEVEL 75)




;; combine a list of insights and sort by decreasing priority
(define (join-insights insights)
  (sort insights > #:key insight-priority))

;; converrt an insight to a jsexpr
(define (insight->jsexpr insight)
  (hash 'm (insight-message insight)
        'p (exact->inexact (insight-priority insight))))

(module+ test
  (require rackunit)
  
  (check-equal? (insight->jsexpr
                 (insight "I'm so awesome!" 75.223))
                (hash 'm "I'm so awesome!"
                      'p 75.223)))

;; formatting of numbers and temperatures

;; format a number for display in an insight
(define (num-format n)
  (number->string (exact->inexact (/ (round (* 10 n)) 10))))

;; format a temperature
(define (temp-format n)
  (string-append (num-format (c2f n)) "°F"))

;; format a *difference* in temperatures
(define (dtemp-format n)
  (string-append (num-format (* 9/5 n)) "°F"))

;; convert a celsius temperature to a fahrenheit one
(define (c2f n)
  (+ 32 (* 9/5 n)))
;; convert a celsius *difference* in temperatures to a fahrenheit one
(define (dc2df n)
  (* 9/5 n))