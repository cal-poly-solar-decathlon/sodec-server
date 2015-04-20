#lang racket/base

(require net/head
         net/url
         json
         "secret.rkt"
         racket/match
         racket/contract)

(provide
 (contract-out [send-reading!
                (-> string? natural? void?)]
               [target-hosts
                (parameter/c (cons/c string? (listof string?)))]))

(define-logger send-reading)

;; these are the hosts to which the readings will be sent.
;; for instance, "localhost:8080". This list should not
;; be empty.
(define target-hosts
  (make-parameter '("localhost:8080")))

(define natural? exact-nonnegative-integer?)

;; send the reading to all current hosts
(define (send-reading! id temp)
  (for ([host (target-hosts)])
    (send-reading!/host host id temp)))

;; after this many seconds, give up on the POST
(define TIMEOUT-SECONDS 3.0)

;; send a reading to one particular host
(define (send-reading!/host host id reading)
  (define result
    (sync/timeout
     TIMEOUT-SECONDS
     (thread
      (lambda ()
        (with-handlers
            ([exn:fail?
              (lambda (exn)
                ;; log error and continue...
                (log-send-reading-error "~a" (exn-message exn)))])
          (log-send-reading-debug
           "sending reading of ~e on device ~e to host ~e\n"
           reading id host)
          (define result
            (remote-call/post 
             (string-append "http://" host "/srv/record-reading?device=" id)
             (jsexpr->bytes (hash 'status reading
                                  'secret SEKRIT))))
          (when (not (string=? result "okay"))
            (log-send-reading-error
             'record-temperature!
             "expected \"okay\" as result, got: ~v"
             result)))))))
  (cond [(eq? #f result)
         (log-send-reading-error "send-reading!: request timed out")]
        [(thread? result) '#t]))


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
         (define reply (car (regexp-match #px".*" body-port)))
         (close-input-port body-port)
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
  (response-port->results (post-impure-port (string->url url-string) post-bytes)))

;; given an input port, return the response code, the first line, the rest of the headers,
;; and the port for the body
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



