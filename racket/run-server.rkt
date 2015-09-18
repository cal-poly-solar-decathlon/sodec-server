#lang racket/base

(require web-server/servlet-env
         racket/runtime-path
         "server.rkt"
         "egauge-monitor.rkt"
         "ip-ping.rkt")

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

(log-client-errors! (build-path here "error.log"))

#;(define HOST "129.65.138.226")
#;(define PORT 9080)

(start-ip-ping #:host "calpolysolardecathlon.org" #:port 80)

(start-egauge-monitor #:host "192.168.1.5" #:port 80)

(start-forecast-monitor)

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