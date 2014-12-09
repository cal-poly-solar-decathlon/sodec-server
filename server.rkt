#lang racket/base

(require racket/match
         racket/runtime-path
         racket/date
         web-server/servlet
         web-server/servlet-env
         json
         "temperatures.rkt"
         "time.rkt")

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

;; handle a timestamp request
(define (handle-timestamp-request)
  (hash 'timestamp (current-timestamp)))

;; handle a device reading request
(define (handle-device-reading-request id)
  (match id
    ["temperature" (handle-temperature-request)]
    [other (response/full 
            404
            #"unknown device name"
            (current-seconds) TEXT/HTML-MIME-TYPE
            (list)
            (list "<html><body><p>"
                  (format "device ~v unknown")
                  "</p></body></html>"))]))

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
              (struct path/param ("reading" (list))))
        (response/xexpr
         (jsexpr->string
          (time (handle-device-reading-request id))))]
       [(list (struct path/param ("srv" (list)))
              (struct path/param ("timestamp" (list))))
        (response/xexpr
         (jsexpr->string (handle-timestamp-request)))]
       ;; a simple 'ping'
       [(list (struct path/param ("srv" (list)))
              (struct path/param ("ping" (list))))
        (response/xexpr 
         (jsexpr->string "alive"))]
       [other
        (response/xexpr
         (format "other: ~v" other))]
     )])
  )

(serve/servlet start
               ;; I see... changing server root path means you need
               ;; your own configuration files....
               ;; #:server-root-path here
               #:extra-files-paths (list htdocs)
               #:servlet-regexp #px"^/srv/.*"
	       #:launch-browser? #f
               #:listen-ip #f
)
