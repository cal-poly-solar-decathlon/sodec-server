#lang racket/base

(require web-server/servlet-env
         racket/runtime-path
         "server.rkt")

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

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