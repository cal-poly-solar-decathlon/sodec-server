#lang racket/base

(require net/head
         net/url
         json
         racket/match
         racket/contract
         "../secret.rkt"
         "../web-funs.rkt")

(provide
 (contract-out [send-reading!
                (-> string? reading? void?)]
               [target-hosts
                (parameter/c (cons/c string? (listof string?)))]
               [send-reading!/core
                (-> string? string? natural? string?)]))

;; a number that can be sent to the database
(define (reading? r)
  (and (exact-integer? r)
       (<= MIN-READING r MAX-READING)))

(define MIN-READING (- (expt 2 63)))
(define MAX-READING (sub1 (expt 2 63)))

(define-logger sodec)

;; these are the hosts to which the readings will be sent.
;; for instance, "localhost:8080". This list should not
;; be empty.
(define target-hosts
  (make-parameter '("localhost:8080")))

(define natural? exact-nonnegative-integer?)

;; send the reading to all current hosts
(define (send-reading! id reading)
  (for ([host (target-hosts)])
    (send-reading!/host host id reading)))

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
                (log-sodec-error "~a" (exn-message exn)))])
          (define result (send-reading!/core host id reading))
          (when (not (string=? result "okay"))
            (log-sodec-error
             'record-temperature!
             "expected \"okay\" as result, got: ~v"
             result)))))))
  (cond [(eq? #f result)
         (log-sodec-error "send-reading!: request timed out")]
        [(thread? result) '#t]))

;; the core reading-sender. Not behind a thread, no timeout, etc.
(define (send-reading!/core host id reading)
  (log-sodec-debug
   "sending reading of ~e on device ~e to host ~e"
   reading id host)
  (define URL-string
    (string-append "http://" host "/srv/record-reading?device=" id))
  (define post-bytes
    (jsexpr->bytes (hash 'status reading
                         'secret SEKRIT)))
  (log-sodec-debug
   "using URL string: ~s" URL-string)
  (log-sodec-debug
   "... and post-bytes: ~s" post-bytes)
  (remote-call/post
   URL-string
   post-bytes))

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

(define lights
  '(s-light-entry-bookend-1A
    s-light-chandelier-1B
    s-light-tv-light-2A
    s-light-kitchen-uplight-3A
    s-light-under-counter-3B
    s-light-pendant-bar-lights-3C
    s-light-bathroom-ambient-4A
    s-light-mirror-4B
    s-light-flexspace-uplight-5A
    s-light-flexspace-cabinet-5B
    s-light-bedroom-uplight-6A
    s-light-bedroom-cabinet-6B
    s-light-porch-lights-8A
    s-light-uplights-and-pot-lights-8B
    ))

#;(parameterize ([target-hosts '("calpolysolardecathlon.org:3000")])
  (send-reading!  "s-temp-lr" 0)
  #;(for ([device (in-list branch-circuit-devices)])
    (send-reading! device 1)))

#;(parameterize ([target-hosts '("calpolysolardecathlon.org:3000")])
  (send-reading! "s-temp-lr" 155))