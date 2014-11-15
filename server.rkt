#lang racket

(require web-server/servlet
         web-server/servlet-env)
 

#;(require web-server/http/xexpr
         #;web-server/http/request-structs)

(define temperature-box (box #f))

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
       [(list (struct path/param ("temperature" (list))))
        (response/xexpr
         (format
          "temperature: ~v" (unbox temperature-box)))]
       [other
        (response/xexpr
         (~a "other: "other))]
     )])
  )

(serve/servlet start
               #:servlet-regexp #px"^.*")
