#lang racket/base

;; web functions used more than once

(require net/url
         net/head
         net/http-client
         net/uri-codec
         json
         racket/match
         racket/contract
         (only-in racket/list add-between))

(provide (contract-out
          [remote-call/get (-> string? number? string? jsexpr?)]
          [remote-call/get/core (-> string? number? string? (list/c bytes? (listof bytes?) input-port?))]
          [remote-call/post (-> string? number? string? bytes? jsexpr?)]
          [remote-call/post/core (-> string? number? string? bytes? (list/c bytes? (listof bytes?) input-port?))])
         results->jsexpr
         sodec-url)

(define-logger sodec)

;; map query associations to a string
(define (query->string assoc)
  (alist->form-urlencoded
   (for/list ([pr (in-list assoc)])
     (match pr
       [(list a (? number? n)) (cons a (number->string n))]
       [(list a (? symbol? n)) (cons a (symbol->string n))]
       [(list a (? string? s)) (cons a s)]))))

;; a string with only alphanum and hyphens OR an exact integer OR a symbol
;; whose corresponding string is alphanum & hyphens
(define (clean-string? str)
  (or (and (string? str)
           (regexp-match #px"^[A-Za-z0-9-]*$" str)
           #t)
      (exact-integer? str)
      (and (symbol? str)
           (clean-string? (symbol->string str)))))


;; formulate a request URL
(define (sodec-url endpoint query)
  (string-append "/srv/" endpoint
                 (cond [query
                        (string-append QUERY-START (query->string query))]
                       [else ""])))

(define QUERY-START "?")

;; given a host, port, and URI, make a GET request and return the
;; result as a jsexpr
(define (remote-call/get host port uri)
  (apply results->jsexpr (remote-call/get/core host port uri)))

;; given a URL, make a POST request and wait for a succesful response, returning a jsexpr
(define (remote-call/post host port uri post-bytes)
  (apply results->jsexpr (remote-call/post/core host port uri post-bytes)))



;; given a URL, make a GET request and wait for a response, returning a jsexpr
(define (remote-call/get/core host port uri)
  (log-sodec-debug "remote-call/get/core: args: ~a"
             (list host port uri))
  (call-with-values (lambda () (http-sendrecv host uri #:port port)) list))


(define (remote-call/post/core host port uri post-bytes)
  (log-sodec-debug "remote-call/post/core: args: ~a" 
                   (list host port uri post-bytes))
  (call-with-values
   (lambda () (http-sendrecv host uri #:port port #:method 'POST
                             #:headers (list #"Content-Type: application/json")
                             #:data post-bytes))
   list))

;; given a list of header strings, find the one with the given name and return
;; its associated right-hand-side
(define (find-field name headers)
  (cond [(null? headers) (error 'find-field "field ~a not found in headers" name)]
        [else (match (car headers)
                [(regexp (bytes-append #"^"(regexp-quote name)
                                       #":[ \t]+(.*)$")
                         (list dc rhs))
                 rhs]
                [other (find-field name (cdr headers))])]))

(module+ test
  (require rackunit)
  
  (check-equal? (find-field #"Content-Type"
                            '(#"Argybargy: 23"
                              #"Content-Type: application/json"
                              #"Troubador: 9"))
                #"application/json"))

;; DEAD:
;; given a URL string, return the response code, the first line, the rest
;; of the headers, and the port for the remainder of the body




;; given a list of results, ensure that the return code is 200 and then parse
;; the body as a jsexpr
(define (results->jsexpr first-line headers response-port)
  (define response-code (extract-response-code first-line))
  (cond [(= response-code 200)
         (define mime-type (find-field #"Content-Type" headers))
         (unless (regexp-match #px#"^application/json(;.*)?$" mime-type)
           (error 'results->jsexpr
                  (format "expected mime type application/json, got ~e"
                          mime-type)))
         (define reply (car (regexp-match #px".*" response-port)))
         (close-input-port response-port)
         (log-sodec-debug (format "reply-bytes : ~v\n" reply))
         (bytes->jsexpr reply)]
        [else
         (error 'results->jsexpr
                "response code: expected 200, got: ~v\nwith message: ~v\nand body: ~v" 
                response-code
                first-line
                (regexp-match #px".*" response-port))]))

;; extract the response code from a http response line:
(define (extract-response-code first-line)
  (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" first-line)
    [(list dc2 response-code-string)
     (string->number (bytes->string/utf-8 response-code-string))]
    [other
     (raise-argument-error 'response-code "HTTP response line matching standard pattern"
                           0 first-line)]))

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


(module+ test
  (require rackunit)

  (check-equal? (clean-string? "hth-t987") #t)
  (check-equal? (clean-string? "hth-t98.7") #f)
  
  (check-equal? (query->string '((device "s-temp-bed")
                                 (start 273)
                                 (end "29")))
                "device=s-temp-bed&start=273&end=29")

  (check-equal? (alist->form-urlencoded
                 '((device . "s-temp-bed")
                   (start . "273")
                   (end . "29")))
                "device=s-temp-bed&start=273&end=29")

  
  (check-equal? (sodec-url "latest-event"
                           `((device s-temp-bed)))
                "/srv/latest-event?device=s-temp-bed")
  (check-equal? (sodec-url "latest-event" #f)
                (string-append "/srv/latest-event"))

  
  (remote-call/get/core "example.com" 80 "/")
  (remote-call/get "calpolysolardecathlon.org" 8080 "/srv/ping")
  )