#lang racket/base

(require racket/contract
         racket/match
         racket/date
         (only-in racket/list add-between))

(provide (contract-out (parse-device-name
                        [->* (string?) (string? string?)])))

(define measurements
  '(temperature humidity electricity_used
                electricity_generated
                light_level
                bogus))

(define temp-lookup
  '((lr living_room)
    (bed bedroom)
    (kit kitchen)
    (out outside)
    (bath bathroom)
    (testing-blackhole testing_blackhole)))

;; given an existing device name, return measurement and new device name
(define (parse-device-name name)
  (match (regexp-split #px"-" name)
    [(list-rest #"s" #"temp" temp-name-list)
     (define temp-name
       (string->symbol
        (apply string-append
               (add-between (map bytes->string/utf-8 temp-name-list) "-"))))
     (match (assoc temp-name
                   temp-lookup)
       [(list dc new-name)
        (list "temperature" (symbol->string new-name))]
       [#f (error 'parse-device-name
                  "no match in table for ~v" temp-name)])]
    [(list #"s" #"hum" hum-name)
     (match (assoc (string->symbol
                    (bytes->string/utf-8
                     hum-name))
                   temp-lookup)
       [(list dc new-name)
        (list "humidity" (symbol->string new-name))]
       [#f (error 'parse-device-name
                  "no match in table for ~v" hum-name)])]
    [(list #"c" #"light" light-name)
     (list "bogus" light-name)]
    [other
     (error 'parse-device-name
            "giving up on ~v" other)]))

(module+ test
  (require rackunit)
  (check-equal? (parse-device-name #"s-temp-lr")
                (list "temperature" "living_room"))
  (check-equal? (parse-device-name #"s-temp-testing-blackhole")
                (list "temperature" "testing_blackhole")))







