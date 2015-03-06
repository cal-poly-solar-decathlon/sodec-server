#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json)

;; given a url and a string, send the string to the URL and wait for a response.
#;(define (remote-evaluator-call/bytes url-string str)
  (define post-bytes (str->post-bytes str))
  (define eval-response-port (post-impure-port (string->url url-string)
                                               post-bytes
                                               standard-headers))
  ;; what about timeouts?
  (define headers (purify-port eval-response-port))
  ;; strange... why do our servers always come back with text/html as 
  ;; a mime type?
  (define reply-code 
    (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" headers)
      [(list match digits) (string->number digits)]
      [other 'unparseable]))
  (cond [(= reply-code 200)
         (define reply (first (regexp-match #px".*" eval-response-port)))
         (close-input-port eval-response-port)
         (log-debug  (format "reply-bytes : ~v\n" reply))
         (bytes->string/utf-8 reply)]
        [else 
         (error 'remote-evaluator-call/bytes
                "response code: expected 200, got: ~v" 
                reply-code)]))

;; given a URL, make a GET request and wait for a response
;; given a url and a string, send the string to the URL and wait for a response.
(define (remote-call/get url-string)
  (match-define (list response-code first-line headers body-port) 
    (remote-call/get/core url-string))
  (cond [(= response-code 200)
         (define mime-type (extract-field "Content-Type" headers))
         (unless (string=? mime-type "application/json")
           (error 'remote-call/get
                  (format "expected mime type application/json, got ~e"
                          mime-type)))
         (define reply (first (regexp-match #px".*" body-port)))
         (close-input-port body-port)
         (log-debug  (format "reply-bytes : ~v\n" reply))
         (bytes->jsexpr reply)]
        [else 
         (error 'remote-call/get
                "response code: expected 200, got: ~v" 
                response-code)]))

;; given a URL string, return the response code, the first line, the rest
;; of the headers, and the port for the remainder of the body
(define (remote-call/get/core url-string)
  (define eval-response-port (get-impure-port (string->url url-string)))
  ;; what about timeouts?
  (define header-string (purify-port eval-response-port))
  ;; strange... why do our servers always come back with text/html as 
  ;; a mime type?
  (match (regexp-match #px"^([^\n]*)\n(.*)" header-string)
    [(list dc first-line headers)
     (match (regexp-match #px"^HTTP/[^ ]* ([0-9]+)" first-line)
       [(list dc2 response-code-string)
        (define reply-code (string->number response-code-string))
        (list reply-code first-line headers eval-response-port)]
       [other
        (error 'remote-call/get/core
               "couldn't extract response code from first response line ~e"
               first-line)])]
    [other (error 'remote-call/get
                  (format "expected response with at least one header line, got ~e"
                          header-string))]))

(run-tests
(test-suite
 "racket evaluator tests"
 (let ()
(define l-u 
  ;; test locally:
  #;"http://localhost:8080"
  ;; test brinckerhoff.org (whatever it points to)
  "http://calpolysolardecathlon.org:8080"
  ;; test new linode
  #;"http://li592-145.members.linode.com:8025")

  
   (define (test-subpath subpath)
     (remote-call/get (string-append l-u "/srv" subpath)))
   
   (check-equal? (test-subpath "/ping") "alive")
   
   (match (test-subpath "/timestamp")
     [(hash-table ('timestamp (? number? n)))
      (check-true (< (find-seconds 0 0 0 1 1 2014) n))]
     [other 
      (fail "timestamp shape")])
   
   ;; simple 404:
   (let ()
     (define 404-call (remote-call/get/core (string-append l-u "/srv/blothints")))
     (check-equal? (first 404-call) 404)
     (check-match
      (first
       (regexp-match #px".*" (fourth 404-call)))
      (regexp #px"blothints")))
   
   ;; near miss on the device name:
   (let ()
     (define 404-call
       (remote-call/get/core
        (string-append l-u "/srv/latest-event?device=uhnoth")))
     (check-equal? (first 404-call) 404)
     (check-match
      (first
       (regexp-match #px".*" (fourth 404-call)))
      (regexp #px#"uhnoth")))
   
   
   (check-equal? (test-subpath "/latest-event?device=s-temp-lr")
                 "no events")
   
   (match (test-subpath "/latest-event?device=s-temp-kit")
     [(hash-table ('timestamp (? number? n))
                  ('device-id "s-temp-kit")
                  ('status (? string? s)))
      (check-true (number? (string->number s)))])
   
   
   
   
)))


