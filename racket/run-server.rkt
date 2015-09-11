#lang racket/base

(require web-server/servlet-env
         racket/runtime-path
         "server.rkt")

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

(log-client-errors! (build-path here "error.log"))

(serve/servlet start
               ;; I see... changing server root path means you need
               ;; your own configuration files....
               ;; #:server-root-path here
               #:extra-files-paths (list htdocs)
               #:servlet-regexp #px"^/srv/.*"
	       #:launch-browser? #f
               #:listen-ip #f
               #:port 3000
               #:log-file (build-path here "server.log")
)