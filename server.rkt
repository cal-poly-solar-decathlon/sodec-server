#lang racket

(require web-server/servlet
         web-server/servlet-env
         racket/runtime-path
         json)

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

(define INITIAL-TEMPERATURE 60)

(define temperature-box (box INITIAL-TEMPERATURE))

(thread 
 (lambda ()
   (let loop ()
     (set-box! temperature-box
               (+ (unbox temperature-box)
                  (/ (- (random 500) 250) 100)))
     (sleep 3)
     (loop))))

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
         (jsexpr->string (hash 'temperature
                               (exact->inexact
                                (unbox temperature-box)))))]
       [other
        (response/xexpr
         (~a "other: "other))]
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
