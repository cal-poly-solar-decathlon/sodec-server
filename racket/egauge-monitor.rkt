#lang racket

(require net/url)

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

(http-sendrecv/url egauge-url)