#lang racket/base

;; web functions used more than once

(require net/url
         net/head
         json
         racket/match)

(provide remote-call/post
         remote-call/post/core
         results->jsexpr
         response-port->results)

(define-logger sodec)


;; given a URL, make a POST request and wait for a succesful response, returning a jsexpr
(define (remote-call/post url-string post-bytes)
  (results->jsexpr (remote-call/post/core url-string post-bytes)))

;; given a list of results, ensure that the return code is 200 and then parse
;; the body as a jsexpr
(define (results->jsexpr results)
  (match-define (list response-code first-line headers body-port) results)
  (cond [(= response-code 200)
         (define mime-type (extract-field "Content-Type" headers))
         (unless (regexp-match #px"^application/json(;.*)?$" mime-type)
           (error 'remote-call/get
                  (format "expected mime type application/json, got ~e"
                          mime-type)))
         (define reply (car (regexp-match #px".*" body-port)))
         (close-input-port body-port)
         (log-sodec-debug  (format "reply-bytes : ~v\n" reply))
         (bytes->jsexpr reply)]
        [else
         (error 'remote-call/get
                "response code: expected 200, got: ~v\nwith message: ~v\nand body: ~v" 
                response-code
                first-line
                (regexp-match #px".*" body-port))]))

;; given a URL string and a POST body, make a POST request, return the response
;; code, the first line, the rest of the headers, and the port for the remainder of the body.
(define (remote-call/post/core url-string post-bytes)
  (log-sodec-debug "remote-call/post/core: url-string ~a, post-bytes: ~a"
             url-string post-bytes)
  (response-port->results (post-impure-port (string->url url-string)
                                            post-bytes
                                            (list
                                             "Content-Type: application/json"))))



;; given an input port, return the response code, the first line, the rest of the
;; headers, and the port for the body
(define (response-port->results response-port)
  (define header-string (purify-port response-port))
  (match (regexp-match #px"^([^\n]*)\n(.*)" header-string)
    [(list dc first-line headers)
     (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" first-line)
       [(list dc2 response-code-string)
        (define reply-code (string->number response-code-string))
        (list reply-code first-line headers response-port)]
       [other
        (error 'remote-call/get/core
               "couldn't extract response code from first response line ~e"
               first-line)])]
    [other (error 'remote-call/get
                  (format "expected response with at least one header line, got ~e"
                          header-string))]))