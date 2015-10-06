#lang racket/base

(require json
         racket/match
         racket/contract
         "../secret.rkt"
         "../web-funs.rkt"
         racket/date)

(provide
 (contract-out [send-reading!
                (-> string? string? reading? void?)]
               [target-hosts
                (parameter/c (cons/c (list/c string? number?) (listof (list/c string? number?))))]
               [fetch-reading
                (-> string? string? reading?)]
               [send-reading!/core
                (-> string? number? string? string? natural? string?)]))

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
  (make-parameter '(("localhost" #f))))

(define natural? exact-nonnegative-integer?)

;; send the reading to all current hosts
(define (send-reading! measurement id reading)
  (for ([host (target-hosts)])
    (send-reading!/host (car host) (cadr host) measurement id reading)))

;; after this many seconds, give up on the POST
(define TIMEOUT-SECONDS 20.0)

;; send a reading to one particular host
(define (send-reading!/host host port measurement id reading)
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
          (define result (send-reading!/core host port measurement id reading))
          (when (not (string=? result "okay"))
            (log-sodec-error
             'record-temperature!
             "expected \"okay\" as result, got: ~v"
             result)))))))
  (cond [(eq? #f result)
         (log-sodec-error "[~v] send-reading!: request timed out, sending ~v"
                          (date->string (seconds->date (current-seconds)) #t)
                          (list host port measurement id reading))]
        [(thread? result) '#t]))

;; the core reading-sender. Not behind a thread, no timeout, etc.
(define (send-reading!/core host port measurement id reading)
  (log-sodec-debug
   "sending reading of ~e on device ~e to host ~e"
   reading id host)
  (define uri
    (sodec-url "record-reading" `((measurement ,measurement)
                                  (device ,id))))
  (define post-bytes
    (jsexpr->bytes (hash 'status reading
                         'secret SEKRIT)))
  (log-sodec-debug
   "using URL args: ~a" (list host port id))
  (log-sodec-debug
   "... and post-bytes: ~s" post-bytes)
  (remote-call/post host port uri post-bytes))

;; fetch the latest reading from a device using the first target host. return 0 if no readings
(define (fetch-reading measurement device)
  (match (remote-call/get (caar (target-hosts))
                          (cadar (target-hosts))
                          (sodec-url "latest-event" `((measurement ,measurement)
                                                      (device ,device))))
    ["no events" 0]
    [(? exact-integer? n) n]
    [other (error 'fetch-reading "unexpected value from latest-event call: ~e" other)]))

