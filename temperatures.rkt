#lang racket/base

(require racket/match
         net/head
         net/url
         json
         "secret.rkt")

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

(define TEMPERATURE-ID "s-temp-bed")
(define TEMP-QUANTA 10)
(define TEMP-ROUNDING (/ 1 TEMP-QUANTA))
(define INITIAL-TEMPERATURE (* 20 TEMP-QUANTA))
(define HOST "calpolysolardecathlon.org:8080")
(define TIMEOUT-SECONDS 3.0)

sync
;; record the temperature
(define (record-temperature! id temp)
  (define result
    (sync/timeout
     TIMEOUT-SECONDS
     (thread
      (lambda ()
        (with-handlers
            ([exn:fail?
              (lambda (exn)
                ;; log error and continue...
                (log-error "~a" (exn-message exn)))])
          (define result
            (remote-call/post 
             (string-append "http://" HOST "/srv/record-reading?device=" id)
             (jsexpr->bytes (hash 'status temp
                                  'secret SEKRIT))))
          (when (not (string=? result "okay"))
            (error
             'record-temperature!
             "expected \"okay\" as result, got: ~v"
             result)))))))
  (cond [(eq? #f result)
         (log-error "record-temperature!: request timed out")]
        [(thread? result) '#t]))

;; BOGUS TEMPERATURE READING THREAD
;; adds a new reading every five seconds
(define (run-mock-thermometer temp-id)
  (thread 
   (lambda ()
     (define temperature-box (box INITIAL-TEMPERATURE))
     (let loop ()
       (set-box! temperature-box
                 (+ (unbox temperature-box)
                    (- (random 5) 2)))
       (thread (lambda () (record-temperature! temp-id (unbox temperature-box))))
       (sleep 5)
       (loop)))))

(run-mock-thermometer "s-temp-bed")
(run-mock-thermometer "s-temp-bath")
(run-mock-thermometer "s-temp-lr")
(run-mock-thermometer "s-temp-out")

;; don't die, just run forever...
(let loop ()
  (sleep 60)
  (loop))



