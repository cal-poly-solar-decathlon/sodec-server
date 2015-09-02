#lang racket

(require net/url
         json)

(string->url "http://localhost:8086/query?db=mydb")

;; express a query as a URL
(define (query->url query)
  (url
   "http"
   #f
   "localhost"
   8086
   #t
   (list (path/param "query" '()))
   `((db . "mydb")
     (q . ,query))
   #f))

(define query
  "SELECT MAX(reading) FROM temperature WHERE device='outside' \
AND time > '2015-06-18' AND time < '2015-06-19' \
GROUP BY time(1h) LIMIT 100")

(define (regexp-member rx l)
  (ormap (λ (h) (regexp-match rx h)) l))

;; perform the query, return the result as a jsexpr
(define (perform-query query)  
  (define-values (status-line headers port)
    (http-sendrecv/url (query->url query)))
  (unless (regexp-match #px"^HTTP/1.1 200" status-line)
    (error 'perform-query
           "expected '200 OK' status line, got: ~v"
           status-line))
  (unless (regexp-member #px#"(?i:Content-Type: +application/json)" headers)
    (error 'perform-query
           "expected content-type application/json, got these headers: ~v"
           headers))
  (read-json port))

  
(match (perform-query query)
  [(hash-table ('results
                (list
                 (hash-table
                  ('series anything)))
                ))
   anything])

