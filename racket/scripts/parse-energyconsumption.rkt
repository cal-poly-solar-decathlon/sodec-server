#lang racket

(require (planet neil/csv:2)
         racket/runtime-path)

(define-runtime-path here ".")

(define make-energy-csv-reader
  (make-csv-reader-maker
   '((quote-char . #\'))))

"http://www.solardecathlon.gov/events/CumulativeEnergyConsumption.csv" 

(define lines
  (call-with-input-file (build-path here "energyconsumption.csv")
    (λ (port)
      (define next (make-energy-csv-reader port))
      (let loop () 
        (match (next)
          [(list) empty]
          [other (cons other (loop))])))))

(define LAST-DATE (first (last lines)))

;; ADJUST THIS MANUALLY...
(define HOURS-DONE (* 7 24))
(define HOURS-TOTAL (- (* 8 24) 1/2))

(define pro-rated-golden (* 175 (/ HOURS-DONE HOURS-TOTAL)))
(define pro-rated-cutoff (* 300 (/ HOURS-DONE HOURS-TOTAL)))
(define pro-rated-points (* 50 (/ HOURS-DONE HOURS-TOTAL)))

(define (to-hundredth x)
  (/ (exact->inexact (round (* 100 x))) 100))

(define team-usages
  (sort
   (for/list ([l (in-list lines)]
              #:when (equal? (car l) LAST-DATE))
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

(define text-from-website
  #<<|
Stevens 		532.747
U at Buffalo 		526.759
Cal Poly 		521.004
Missouri S&T 		515.874
Crowder/Drury 		494.084
Clemson 		493.430
Sacramento State 		489.319
Texas/Germany 		473.784
Team NY Alfred 		470.102
UC Davis 		450.270
Team Orange County 		440.846
West Virginia/Rome 		431.610
NY City Tech 		339.167
Mass/Central America 		326.610
|
  )

(define data-from-website
  (for/list ([line (regexp-split #px"\n" text-from-website)])
    (match (regexp-split #px" *\t+" line)
      [(list team score)
       (list team (string->number score))])))

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

