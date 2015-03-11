#lang racket/base

(require racket/match
         web-server/servlet
         json
         racket/date
         "data-model.rkt"
         "ids.rkt"
         xml)

(provide start)

(define SEKRIT "$a8Es#crB469")

;; handle a request. "front door" of the server
(define (start req)
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
           (time (handle-device-latest-event-request
                  (url-query uri)))]
          ;; events in a range for a sensor
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("events-in-range" (list))))
           (time (handle-device-events-in-range-request
                  (url-query uri)))]
          ;; timestamp of the server
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("timestamp" (list))))
           (response/json (handle-timestamp-request))]
          ;; a simple 'ping'
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("ping" (list))))
           (response/json "alive")]
          [other
           (404-response
            #"unknown server path"
            (format "GET url ~v doesn't match known pattern" (url->string uri)))])]
       [#"POST"
        (match (url-path uri)
          ;; record a new reading
          [(list (struct path/param ("srv" (list)))
                 (struct path/param ("record-reading" (list))))
           (time (handle-new-reading (url-query uri) post-data/raw))]
          [other
           (404-response
            #"unknown server path"
            (format "POST url ~v doesn't match known pattern" (url->string uri)))])])]))

;; handle a timestamp request
(define (handle-timestamp-request)
  (hash 'timestamp (date->seconds (current-timestamp))))

;; handle a device reading request
(define (handle-device-latest-event-request query)
  (match query
    [(list (cons 'device (? ID? id)))
     (response/json
      (maybe-event->jsexpr (sensor-latest-event id)))]
    [(list (cons 'device bad-device))
     (404-response
      #"unknown device name"
      (format "device ~v unknown" bad-device))]
    [else
     (404-response
      #"wrong query fields"
      (format "expected a query with fields matching spec, got: ~v"
              query))]))

;; handle a device time range reading request
(define (handle-device-events-in-range-request query)
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
            (404-response
             #"range too long"
             (format "expected a query length of less than one day, got ~e"
                     (- end start)))]
           [else
            (response/json
             (events->jsexpr 
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

;; handle an incoming reading
(define (handle-new-reading query post-data)
  (match query
    [(list (cons 'device (? ID? id)))
     (with-handlers ([(lambda (exn)
                        (and (exn:fail? exn)
                             (regexp-match #px"bytes->jsexpr" (exn-message exn))))
                      (lambda (exn)
                        (fail-response
                         400
                         #"bad JSON in POST"
                         (format "expected POST bytes parseable as JSON, got: ~e"
                                 post-data)))])
       (define jsexpr (bytes->jsexpr post-data))
       (match jsexpr
         [(hash-table ('status (? integer? reading))
                      ('secret (? string? secret)))
          (cond [(string=? secret SEKRIT)
                 (record-sensor-status! id (inexact->exact reading))
                 (response/json "okay")]
                [else 
                 (fail-response
                  403
                  #"wrong secret"
                  "request didn't come with the right secret")])]
         [else 
          (fail-response
           400
           #"wrong JSON in POST"
           (format "expected JSON data matching spec, got: ~e"
                   jsexpr))]))]
    [(list (cons 'device bad-device))
     (404-response
      #"unknown device name"
      (format "device ~v unknown" bad-device))]
    [other
     ;; spent a while on stack overflow checking what response code is
     ;; best, seems there's quite a bit of disagreement...
     (404-response
      #"wrong query fields"
      (format "expected a query with fields matching spec, got: ~e"
              query))]))

(define NUM-REGEXP #px"^[[:digit:]]+$")

(define DAY-SECONDS 86400)
(define nat? exact-nonnegative-integer?)

;; issue a 404 response:
(define (404-response header-msg body-msg)
  (fail-response 404 header-msg body-msg))

;; issue a failure response
(define (fail-response code header-msg body-msg)
  (response/full
   code
   header-msg
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list)
   (list
    (string->bytes/utf-8
     (xexpr->string
      `(html (body (p ,body-msg))))))))


;; a successful json response
;; jsexpr -> response
(define (response/json jsexpr)
  (response/full
   200 #"Okay"
   (current-seconds) #"application/json"
   null
   (list (jsexpr->bytes jsexpr))))


(module+ test
  (require rackunit)
  
  (check-match (jsexpr->bytes (handle-timestamp-request))
               (regexp #px#"^\\{\"timestamp\":[[:digit:]]+\\}$"))
  
  (check-equal? 3 3))