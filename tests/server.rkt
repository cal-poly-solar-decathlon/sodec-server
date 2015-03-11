#lang racket

(require net/url
         net/head
         rackunit
         rackunit/text-ui
         racket/date
         json)

;; given a URL, make a GET request and wait for a response, returning a jsexpr
(define (remote-call/get url-string)
  (results->jsexpr (remote-call/get/core url-string)))

;; given a URL, make a POST request and wait for a succesful response, returning a jsexpr
(define (remote-call/post url-string post-bytes)
  (results->jsexpr (remote-call/post/core url-string post-bytes)))

;; given a list of results, ensure that the return code is 200 and then parse
;; the body as a jsexpr
(define (results->jsexpr results)
  (match-define (list response-code first-line headers body-port) results)
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
                "response code: expected 200, got: ~v\nwith message: ~v\nand body: ~v" 
                response-code
                first-line
                (regexp-match #px".*" body-port))]))

;; given a URL string, return the response code, the first line, the rest
;; of the headers, and the port for the remainder of the body
(define (remote-call/get/core url-string)
  (response-port->results (get-impure-port (string->url url-string))))

;; given a URL string and a POST body, make a POST request, return the response
;; code, the first line, the rest of the headers, and the port for the remainder of the body.
(define (remote-call/post/core url-string post-bytes)
  (response-port->results (post-impure-port (string->url url-string) post-bytes)))

;; given an input port, return the response code, the first line, the rest of the headers,
;; and the port for the body
(define (response-port->results response-port)
  ;; what about timeouts?
  (define header-string (purify-port response-port))
  ;; strange... why do our servers always come back with text/html as 
  ;; a mime type?
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

(run-tests
(test-suite
 "racket evaluator tests"
 (let ()
(define l-u 
  ;; test locally:
  "http://localhost:8080"
  ;; test brinckerhoff.org (whatever it points to)
  #;"http://calpolysolardecathlon.org:8080"
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
   
   
   (check-equal? (test-subpath "/latest-event?device=s-temp-bogus")
                 "no events")
   
   (check-match
    (test-subpath "/latest-event?device=s-temp-bed")
    (hash-table ('timestamp (? number? n))
                  ('device-id "s-temp-bed")
                  ('status (? number? s))))
   
   
   (check-true
    (let ([result
           (test-subpath "/events-in-range?device=s-temp-bed;start=14;end=19")])
      (andmap (lambda (elt)
                (match elt
                  [(hash-table ('timestamp (? number? n))
                               ('device-id "s-temp-bed")
                               ('status (? number? s)))
                   #t]))
              result)))
   
   
   
   
   
   
)))


