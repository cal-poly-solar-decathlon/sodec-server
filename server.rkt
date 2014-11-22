#lang racket/base

(require racket/match
         racket/runtime-path
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
              (struct path/param ("temperature" (list))))
        (response/xexpr
         (jsexpr->string 
          (time (handle-temperature-request))))]
       [(list (struct path/param ("srv" (list)))
              (struct path/param ("timestamp" (list))))
        (response/xexpr
         (jsexpr->string (handle-timestamp-request)))]
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
