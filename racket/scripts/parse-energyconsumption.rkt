#lang racket

(require (planet neil/csv:2)
         racket/runtime-path)

(define-runtime-path here ".")

(define make-energy-csv-reader
  (make-csv-reader-maker
   '((quote-char . #\'))))

(define lines
  (call-with-input-file (build-path here "energyconsumption.csv")
    (λ (port)
      (define next (make-energy-csv-reader port))
      (let loop () 
        (match (next)
          [(list) empty]
          [other (cons other (loop))])))))

(define HOURS-DONE (+ 10 (* 6 24)))
(define HOURS-TOTAL (- (* 8 24) 1/2))

(define pro-rated-golden (* 175 (/ HOURS-DONE HOURS-TOTAL)))
(define pro-rated-cutoff (* 300 (/ HOURS-DONE HOURS-TOTAL)))
(define pro-rated-points (* 50 (/ HOURS-DONE HOURS-TOTAL)))

(define (to-hundredth x)
  (/ (exact->inexact (round (* 100 x))) 100))

(define team-usages
  (sort
   (for/list ([l (in-list lines)]
              #:when (equal? (car l) "2015-10-14 20:45:00"))
     (define usage (string->number (fourth l)))
     (define score (cond [(< usage pro-rated-golden) pro-rated-points]
                         [(< usage pro-rated-cutoff)
                          (* pro-rated-points
                             (- 1
                                (/ (- usage pro-rated-golden)
                                   (- pro-rated-cutoff pro-rated-golden))))]
                         [else 0]))
     (list (third l) (to-hundredth usage)
           (to-hundredth score)))
   >
   #:key third))

(define data-from-website
  '(("Stevens" 272.282)
    ("U at Buffalo" 270.794)
    ("Missouri S&T" 268.139)
    ("Cal Poly" 266.216)
    ("Team NY Alfred" 254.099)
    ("Crowder/Drury" 251.019)
    ("Sacramento State" 247.884)
    ("West Virginia/Rome" 246.182)
    ("Clemson" 245.600)
    ("UC Davis" 232.213)
    ("Team Orange County" 226.686)
    ("Texas/Germany" 223.597)
    ("Mass/Central America" 156.709)
    ("NY City Tech" 130.065)))

(for/list ([t (in-list data-from-website)])
  (display
   (apply
    string-append
    (add-between
     (map ~a
          (cons (first t) (cons (second t)
                                (rest (assoc (first t) team-usages)))))
     ",")))
  
   (newline))

