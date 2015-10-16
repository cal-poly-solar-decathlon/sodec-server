#lang racket

(define output
  #<<|
2015-10-11T00:00:00Z	-5451363
2015-10-11T04:00:00Z	-5459827
2015-10-11T08:00:00Z	-5459895
2015-10-11T12:00:00Z	-5466451
2015-10-11T16:00:00Z	-5474573
2015-10-11T20:00:00Z	-10279488
2015-10-12T00:00:00Z	-19810610
2015-10-12T04:00:00Z	-29333120
2015-10-12T08:00:00Z	-33510685
2015-10-12T12:00:00Z	-33517162
2015-10-12T16:00:00Z	-33524737
2015-10-12T20:00:00Z	-33543369
2015-10-13T00:00:00Z	-33553994
2015-10-13T04:00:00Z	-33561861
2015-10-13T08:00:00Z	-33569283
2015-10-13T12:00:00Z	-33577518
2015-10-13T16:00:00Z	-33582787
2015-10-13T20:00:00Z	-33603179
2015-10-14T00:00:00Z	-34159516
2015-10-14T04:00:00Z	-43646673
2015-10-14T08:00:00Z	-53103271
2015-10-14T12:00:00Z	-62601632
2015-10-14T16:00:00Z	-72166350
2015-10-14T20:00:00Z	-72230442
2015-10-15T00:00:00Z	-72246345
2015-10-15T04:00:00Z	-77571035
2015-10-15T08:00:00Z	-87255475
2015-10-15T12:00:00Z	-97223018
2015-10-15T16:00:00Z	-104734992
|
)

(define data
  (for/list ([line (regexp-split #px"\n" output)])
    (match (regexp-split #px"\t" line)
      [(list date val)
       (list date  (string->number val))])))
(for/list ([d data]
           [e (rest data)])
  (printf "~a\t~a\n"
          (first e) (- (second e) (second d)))
  (list (first e) (- (second e) (second d))))