#lang racket/base

(require racket/match
         web-server/servlet
         net/uri-codec
         json
         racket/date
         racket/contract
         "data-model-influx.rkt"
         #;"ids.rkt"
         "secret.rkt"
         "id-to-measurement-and-device.rkt"
         xml)

(provide start
         (contract-out
          [log-client-errors! (-> path-string? void?)]))

(define-logger client)

;; handle a request. "front door" of the server
(define (start req)
  (with-handlers ;; log all errors...
      ([(lambda (exn) #t)
        (lambda (exn)
          (log-error (exn-message exn))
          server-fail-response)])
  (match req
    [(struct request
       (method
        uri
        headers/raw
        bindings/raw-promise
        post-data/raw
        host-ip
        host-port
        client-ip))
     (match method
       [#"GET"
        (match (url-path uri)
          ;; latest event for a sensor
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("latest-event" (list))))
           (handle-device-latest-event-request
            (url-query uri))]
          ;; events in a range for a sensor
          #;[(list (struct path/param ("srv" (list)))
                 (struct path/param ("events-in-range" (list))))
           (handle-device-events-in-range-request
            (url-query uri))]
          ;; number of events in a range for a sensor
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("count-events-in-range" (list))))
           (handle-device-count-events-in-range-request
            (url-query uri))]
          ;; a list of all devices
          ;; removed for now
          #;[(list (struct path/param ("srv" (list)))
                 (struct path/param ("list-devices" (list))))
           (handle-device-list-request)]
          ;; timestamp of the server
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("timestamp" (list))))
           (response/json (handle-timestamp-request))]
          ;; a simple 'ping'
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("ping" (list))))
           (response/json "alive")]
          ;; a deliberate exception for testing logging
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("divbyzero" (list))))
           (/ 1 0)]
          [other
           (define message
             (format "GET url ~v doesn't match known pattern" (url->string uri)))
           (log-warning message)
           (404-response
            #"unknown server path"
            message)])]
       [#"POST"
        (match (url-path uri)
          ;; record a new reading
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("record-reading" (list))))
           (handle-new-reading (url-query uri) headers/raw post-data/raw)]
          [other
           (404-response
            #"unknown server path"
            (format "POST url ~v doesn't match known pattern" (url->string uri)))])])])))

;; handle a timestamp request
(define (handle-timestamp-request)
  (hash 'timestamp (current-timestamp)))

;; handle a device list request
;; removed for now...
#;(define (handle-device-list-request)
  (response/json (devices-list)))

;; handle a device reading request
(define (handle-device-latest-event-request query)
  (match query
    [(list-no-order (cons 'measurement (? string? measurement))
                    (cons 'device (? string? device)))
     (with-handlers ([(lambda (exn)
                        (regexp-match #px"expected: " (exn-message exn)))
                      (lambda (exn)
                        (fail-response
                         404
                         #"bad arguments"
                         (exn-message exn)))])
       (response/json
        (maybe-reading->jsexpr (sensor-latest-reading measurement device))))]
    [else
     (404-response
      #"wrong query fields"
      (format "expected a query with fields matching spec, got: ~v"
              query))]))

;; handle a device time range reading request
#;(define (handle-device-events-in-range-request query)
  (match query
    ;; could give more fine-grained error messages here...
    [(list-no-order
      (cons 'device (? ID? id))
      (cons 'start (regexp NUM-REGEXP (list start-str)))
      (cons 'end (regexp NUM-REGEXP (list end-str))))
     (define start (string->number start-str))
     (define end (string->number end-str))
     (cond [(< end start)
            (404-response
             #"end before start"
             (format "expected a query with start <= end, got: ~e"
                     query))]
           [(< DAY-SECONDS (- end start))
            (fail-response
             400
             #"range too long"
             (format "expected a query length of less than or equal to one day, got ~e"
                     (- end start)))]
           [else
            (response/json
             (events->jsexpr/short
              (sensor-events-in-range id 
                                      (seconds->date start)
                                      (seconds->date end))))])]
    [else
     ;; spent a while on stack overflow checking what response code is
     ;; best, seems there's quite a bit of disagreement...
     (404-response
      #"wrong query fields"
      (format "expected a query with fields matching spec, got: ~e"
              query))]))

;; handle a device time range COUNT reading request
;; possible abstraction...?
(define (handle-device-count-events-in-range-request query)
  (match query
    ;; could give more fine-grained error messages here...
    [(list-no-order
      (cons 'measurement (? string? measurement))
      (cons 'device (? string? device))
      (cons 'start (regexp NUM-REGEXP (list start-str)))
      (cons 'end (regexp NUM-REGEXP (list end-str))))
     (define start (string->number start-str))
     (define end (string->number end-str))
     (cond [(< end start)
            (404-response
             #"end before start"
             (format "expected a query with start <= end, got: ~e"
                     query))]
           [else
            (response/json
             (count-sensor-events-in-range
              measurement
              device
              start
              end))])]
    [else
     ;; spent a while on stack overflow checking what response code is
     ;; best, seems there's quite a bit of disagreement...
     (404-response
      #"wrong query fields"
      (format "expected a query with fields matching spec, got: ~e"
              query))]))

;; handle an incoming reading
(define (handle-new-reading query headers post-data)
  (match query
    [(list-no-order (cons 'measurement (? string? measurement))
                    (cons 'device (? string? device)))
     (handle-new-reading/md measurement device headers post-data)]
    [(list (cons 'device (? string? id)))
     ;; HANDLING FOR LEGACY DEVICE ID SCHEME:
     (define-values (measurement device) (parse-device-name id))
     (handle-new-reading/md measurement device headers post-data)]
    [other
     ;; spent a while on stack overflow checking what response code is
     ;; best, seems there's quite a bit of disagreement...
     (404-response
      #"wrong query fields"
      (format "expected a query with fields matching spec, got: ~e"
              query))]))

(define (handle-new-reading/md measurement device headers post-data)
  (with-handlers ([(lambda (exn)
                     (and (exn:fail? exn)
                          (regexp-match #px"bytes->jsexpr" (exn-message exn))))
                   (lambda (exn)
                     (fail-response
                      400
                      #"bad JSON in POST"
                      (format "expected POST bytes parseable as JSON, got: ~e"
                              post-data)))])
    (match
        (ormap (λ (h) (and (equal? (header-field h) #"Content-Type")
                           (header-value h)))
               headers)
      [#"application/json"
       (define jsexpr (bytes->jsexpr post-data))
       (match jsexpr
         [(hash-table ('status (? integer? reading))
                      ('secret (? string? secret)))
          (handle-new-reading/mdss measurement device reading secret)]
         [else 
          (fail-response
           400
           #"wrong JSON in POST"
           (format "expected JSON data matching spec, got: ~e"
                   jsexpr))])]
      [#"application/x-www-form-urlencoded"
       (match (form-urlencoded->alist (bytes->string/utf-8 post-data))
         [(list-no-order
           (cons 'status (? string? (regexp #px"^(-)?[[:digit:]]+$"
                                            (list reading dc))))
           (cons 'secret (? string? secret)))
          (handle-new-reading/mdss measurement device (string->number reading) secret)]
         [other (fail-response
                 400
                 #"wrong fields"
                 (format
                  "expected well-formatted post bytes, got: ~e"
                  post-data))])]
      [(? bytes? otherbytes)
       (fail-response
        400
        #"unknown content type"
        (format "expected Content-Type: application/json or application/x-www-form-urlencoded, got: ~a"
                otherbytes))]
      [#f
       (fail-response
        400
        #"no content-type specified"
        (format "expected request with Content-Type header"))])
    ))

;; given a measurement, a device, a reading, and a secret, record the reading
(define (handle-new-reading/mdss measurement device reading secret)
  (cond [(string=? secret SEKRIT)
         (with-handlers ([(lambda (exn)
                            (regexp-match #px"expected: " (exn-message exn)))
                          (lambda (exn)
                            (fail-response
                             404
                             #"bad arguments"
                             (exn-message exn)))])
           (record-sensor-status! measurement device (inexact->exact reading))
           (response/json "okay"))]
        [else 
         (fail-response
          403
          #"wrong secret"
          "request didn't come with the right secret")]))

(define NUM-REGEXP #px"^[[:digit:]]+$")

(define HOUR-SECONDS 3600)
(define DAY-SECONDS 86400)
(define nat? exact-nonnegative-integer?)

;; issue a 404 response:
(define (404-response header-msg body-msg)
  (fail-response 404 header-msg body-msg))

;; issue a failure response
(define (fail-response code header-msg body-msg)
  (log-client-error
   (format "[~a] [~a] ~a"
           (date->string (seconds->date (current-seconds)) #t)
           code
           body-msg))
  (response/full
   code
   header-msg
   (current-seconds) TEXT/HTML-MIME-TYPE
   ;; need CORS headers even on fails to allow them to be read...
   (list (make-header #"Access-Control-Allow-Origin"
                      #"*"))
   (list
    (string->bytes/utf-8
     (xexpr->string
      `(html (body (p ,body-msg))))))))

;; a generic server exception 500:
(define server-fail-response
  (fail-response 500 #"server exception"
                 "Server-side exception. Check logs for more detail."))


;; a successful json response
;; jsexpr -> response
(define (response/json jsexpr)
  (response/full
   200 #"Okay"
   (current-seconds)
   #"application/json"
   (list (make-header #"Access-Control-Allow-Origin"
                      #"*"))
   (list (jsexpr->bytes jsexpr))))


;; find a member of the list matching the regexp
(define (regexp-member rx l)
  (ormap (λ (h) (regexp-match rx h)) l))

;; create a log-receiver for client errors, pipe them to a file
(define (log-client-errors! logfile)
  (define log-receiver (make-log-receiver
                        (current-logger)
                        'info
                        'client))
  (define errorlog-port
    (open-output-file logfile #:exists 'append))
  (thread
   (λ ()
     (let loop ()
       (match (sync log-receiver)
         [(vector level msg data topic)
          (fprintf errorlog-port "~a\n" msg)
          (flush-output errorlog-port)])
       (loop))))
  (void))

(module+ test
  (require rackunit)
  
  (check-match (jsexpr->bytes (handle-timestamp-request))
               (regexp #px#"^\\{\"timestamp\":[[:digit:]]+\\}$"))

  (check-equal? (regexp-member #px"ad*e" (list "ab" "adddde" "aq"))
                '("adddde")))