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
           "sending reading of ~e on device ~e to host ~e"
           reading id host)
          (define URL-string
            (string-append "http://" host "/srv/record-reading?device=" id))
          (define post-bytes
            (jsexpr->bytes (hash 'status reading
                                 'secret SEKRIT)))
          (log-send-reading-debug
           "using URL string: ~s" URL-string)
          (log-send-reading-debug
           "... and post-bytes: ~s" post-bytes)
          (define result
            (remote-call/post
             URL-string
             post-bytes))
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


;; setting unused lights, just once...
(define branch-circuit-devices
    '("s-elec-used-laundry"
  "s-elec-used-dishwasher"
  "s-elec-used-refrigerator"
  "s-elec-used-induction-stove"
  "s-elec-used-ewh-solar-water-heater"
  "s-elec-used-kitchen-receps-1"
  "s-elec-used-kitchen-receps-2"
  "s-elec-used-living-receps"
  "s-elec-used-dining-receps-1"
  "s-elec-used-dining-receps-2"
  "s-elec-used-bathroom-receps"
  "s-elec-used-bedroom-receps-1"
  "s-elec-used-bedroom-receps-2"
  "s-elec-used-mechanical-receps"
  "s-elec-used-entry-receps"
  "s-elec-used-exterior-receps"
  "s-elec-used-grey-water-pump-recep"
  "s-elec-used-black-water-pump-recep"
  "s-elec-used-thermal-loop-pump-recep"
  "s-elec-used-water-supply-pump-recep"
  "s-elec-used-water-supply-booster-pump-recep"
  "s-elec-used-vehicle-changing-recep"
  "s-elec-used-heat-pump-recep"
  "s-elec-used-air-handler-recep"))

#;(parameterize ([target-hosts '("calpolysolardecathlon.org:8080")])
  (for ([device (in-list branch-circuit-devices)])
    (send-reading! device 0)))

#;(parameterize ([target-hosts '("calpolysolardecathlon.org:3000")])
  (send-reading! "s-temp-lr" 155))