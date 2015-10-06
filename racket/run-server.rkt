#lang racket/base

(require web-server/servlet-env
         racket/runtime-path
         "server.rkt"
         "egauge-monitor.rkt"
         "ip-ping.rkt"
         "network-config.rkt"
         (only-in "data-model.rkt" echo-data-to-host))

(define-runtime-path here ".")
(define-runtime-path htdocs "./htdocs")

(log-client-errors! (build-path here "error.log"))

(echo-data-to-host '("calpolysolardecathlon.org" 3000))

(start-ip-ping #:host PING-HOST #:port 80)

(start-forecast-monitor)

;; duplicated device names will cause problems...
(start-egauge-monitor #:host EGAUGE-1 #:port 80)
(start-egauge-monitor #:host EGAUGE-2 #:port 80)

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