#lang racket/base

(require racket/match
         racket/runtime-path
         racket/date
         web-server/servlet
         web-server/servlet-env
         json
         "data-model.rkt"
         "temperatures.rkt"
         "time.rkt"
         "ids.rkt"
         xml)

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

;; handle a timestamp request
(define (handle-timestamp-request)
  (hash 'timestamp (current-timestamp)))

;; handle a device reading request
(define (handle-device-latest-event-request query)
  (define query-fields (map car query))
  (cond
    [(equal? query-fields '(device))
     (define id (cdr (assoc 'device query)))
     (cond [(ID? id)
            (response/json
             (maybe-event->jsexpr (sensor-latest-event id)))]
           [else
            (404-response
             #"unknown device name"
             (format "device ~v unknown" id))])]
    [else
     (404-response
      #"wrong query fields"
      (format "expected a query with exactly these query fields: (id), got: ~v"
              query-fields))]))

;; issue a 404 response:
(define (404-response header-msg body-msg)
  (response/full
   404
   header-msg
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list)
   (list
    (string->bytes/utf-8
     (xexpr->string
      `(html (body (p ,body-msg))))))))



;; handle a request
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
     
     (match (url-path uri)
       [(list (struct path/param ("srv" (list)))
              (struct path/param ("latest-event" (list))))
        (time (handle-device-latest-event-request
               (url-query uri)))]
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
         (format "uri ~v doesn't match known pattern" (url->string uri)))]
     )])
  )

;; a successful json response
;; jsexpr -> response
(define (response/json jsexpr)
  (response/full
   200 #"Okay"
   (current-seconds) #"application/json"
   null
   (list (jsexpr->bytes jsexpr))))

(serve/servlet start
               ;; I see... changing server root path means you need
               ;; your own configuration files....
               ;; #:server-root-path here
               #:extra-files-paths (list htdocs)
               #:servlet-regexp #px"^/srv/.*"
	       #:launch-browser? #f
               #:listen-ip #f
               #:port 8080
               #:log-file (build-path here "server.log")
)
