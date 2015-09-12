#lang racket/base

(require net/url
         racket/match
         racket/contract
         racket/date
         sxml
         "data-model-influx.rkt")

(define-logger sodec)

(date-display-format 'iso-8601)

(provide (contract-out
          [start-egauge-monitor (-> #:host string?
                                    #:port exact-integer?
                                    thread?)]))

(define ENDPOINT (list "cgi-bin" "egauge"))

(define EGAUGE-SLEEP-INTERVAL 15)

(define (start-egauge-monitor #:host host #:port port)
  (define egauge-url
    (url "http" #f ;; user
     host port
     #t ;; absolute?
     (for/list ([element ENDPOINT])
       (path/param element '()))
     null #f ;;fragment
     ))

  (thread 
   (lambda ()
     (let loop ()
       (thread
        (lambda ()
          (with-handlers ([(lambda (exn) #t)
                           (lambda (exn)
                             (log-sodec-error
                              "~a : error during egauge fetch:\n~a"
                              (date->string (seconds->date
                                             (current-seconds))
                                            #t)
                              (exn-message exn)))])
          (fetch-and-record-egauge-readings! egauge-url))))
       (sleep EGAUGE-SLEEP-INTERVAL)
       (loop)))))





#;(define egauge-current-seconds
  (match ((sxpath '(data ts)) egauge-status)
    [(list (list 'ts ts-string)) (string->number ts-string)]))

;; for some reason, this is huge. NTP updating on egauge is not working,
;; not sure why.
#;(define egauge-difference
  (- (current-seconds) egauge-current-seconds))



;; WE ASSUME THAT THE EGAUGE CAN'T HAVE TWO DEVICES WITH THE SAME NAME

;; this mapping will have the property that it's stable; for a given set
;; of names, the same mapping will be produced. So, for instance, a set
;; containing ABC+ and ABC* and ABC_ will produce this mapping:
;; ABC* => abc_
;; ABC+ => abc__1
;; ABC_ => abc__2
;; ... but if you change the set of names, all bets are off. Doing this
;; "right" would require some more careful name-mangling

(define (simple-tidy-name name)
  (regexp-replace* #px"[^_A-Za-z0-9]"
                   (string-downcase name) "_"))

;; given a set of names, produce an a-list mapping names to
;; legal device names
(define (name-mapping names)
  (define sorted-names (sort names string<?))
  (define mappings
    (reverse
     (for/fold ([so-far null])
               ([name (in-list sorted-names)])
       (define base (simple-tidy-name name))
       (let loop ([n 0])
         (define this-try
           (cond [(= n 0) base]
                 [else (string-append base "_" (number->string n))]))
         (cond [(member this-try so-far)
                (loop (add1 n))]
               [else
                (cons this-try so-far)])))))
  (map list sorted-names mappings))

;; given an association list from names to names and a reading,
;; write the reading to influx
(define (write-egauge-reading mapping reading timestamp)
  (match (list ((sxpath '(@ n)) reading)
               ((sxpath '(v)) reading))
    [(list (list (list 'n name)) (list (list 'v value-str)))
     (define influx-device-name
       (cadr (assoc name mapping)))
     (define reading (string->number value-str))
     (record-sensor-status! "electric_power" influx-device-name reading
                            #:timestamp timestamp)]))

;; read the data from the egauge, write it to influx
(define (fetch-and-record-egauge-readings! egauge-url)
  (define timestamp (current-seconds))
  (define-values (status-line headers port)
    (http-sendrecv/url egauge-url))
  (unless (regexp-match #px"^HTTP/1.1 200" status-line)
    (error 'fetch-and-record-egauge-readings
           "expected 200 status from egauge, got status line ~e and content ~e"
           status-line
           (regexp-match #px".*" port)))  
  (define egauge-status (ssax:xml->sxml port '()))  
  (define readings ((sxpath '(data r)) egauge-status))
  (define names (map cadr ((sxpath '(data r @ n)) egauge-status)))
  (define mapping (name-mapping names))
  (for ([reading (in-list readings)])
    (write-egauge-reading mapping reading timestamp)))

(module+ test
  (require rackunit)
  (check-equal? (name-mapping '("ABC+" "ABC*" "ABC_"))
                '(("ABC*" "abc_")
                  ("ABC+" "abc__1")
                  ("ABC_" "abc__2"))))



