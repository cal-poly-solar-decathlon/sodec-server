#lang racket

(require net/url
         sxml)

(define HOST "129.65.138.226")
(define PORT 9080)

(define ENDPOINT (list "cgi-bin" "egauge"))

(define egauge-url
  (url
   "http"
   #f ;; user
   HOST
   PORT
   #t ;; absolute
   (for/list ([element ENDPOINT])
     (path/param element '()))
   null
   #f ;;fragment
   ))

(define-values (status-line headers port)
  (http-sendrecv/url egauge-url))

(define egauge-status
  (ssax:xml->sxml port '()))

(define egauge-current-seconds
  (match ((sxpath '(data ts)) egauge-status)
    [(list (list 'ts ts-string)) (string->number ts-string)]))

;; for some reason, this is huge. NTP updating on egauge is not working,
;; not sure why.
(define egauge-difference
  (- (current-seconds) egauge-current-seconds))

egauge-status

((sxpath '(data r)) egauge-status)