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
         "ids.rkt")

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

;; handle a timestamp request
(define (handle-timestamp-request)
  (hash 'timestamp (current-timestamp)))

;; handle a device reading request
(define (handle-device-latest-event-request id)
  (cond [(ID? id)
         (response/json
          (maybe-event->jsexpr (sensor-latest-event id)))]
        [else (response/full 
               404
               #"unknown device name"
               (current-seconds) TEXT/HTML-MIME-TYPE
               (list)
               (list #"<html><body><p>"
                     (string->bytes/utf-8 (format "device ~v unknown" id))
                     #"</p></body></html>"))]))



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
              (struct path/param ("device" (list)))
              (struct path/param (id (list)))
              (struct path/param ("latest-event" (list))))
        (time (handle-device-latest-event-request id))]
       [(list (struct path/param ("srv" (list)))
              (struct path/param ("timestamp" (list))))
        (response/json (handle-timestamp-request))]
       ;; a simple 'ping'
       [(list (struct path/param ("srv" (list)))
              (struct path/param ("ping" (list))))
        (response/json "alive")]
       [other
        (response/full 
         404
         #"unknown server path"
         (current-seconds) TEXT/HTML-MIME-TYPE
         (list)
         (list #"<html><body><p>"
               (string->bytes/utf-8 (format "uri ~v doesn't match known pattern" uri))
               #"</p></body></html>"))]
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
