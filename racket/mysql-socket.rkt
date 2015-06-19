#lang racket/base

(require db/mysql
         racket/runtime-path
         racket/file
         racket/string)

(provide mysql-socket)

(define-runtime-path here ".")

(define mysql-socket
  (cond [(file-exists? (build-path here "mysql-socket"))
         (string-trim (file->string (build-path here "mysql-socket")))]
        [else
         (mysql-guess-socket-path)]))

