#lang racket

(require net/url
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
  (define eval-response-port (get-impure-port (string->url url-string)))
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
         (bytes->jsexpr reply)]
        [else 
         (error 'remote-evaluator-call/bytes
                "response code: expected 200, got: ~v" 
                reply-code)]))

(run-tests
(test-suite
 "racket evaluator tests"
 (let ()
(define l-u 
  ;; test locally:
  "http://localhost:8000"
  ;; test brinckerhoff.org (whatever it points to)
  #;"http://brinckerhoff.org:8025/"
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
   
   
   
   
)))


