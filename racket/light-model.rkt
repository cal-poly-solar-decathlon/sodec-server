#lang racket

(require math/distributions)

;; modeling light usage.

;; we're going to imagine that people arrive independently, and
;; that they stay for a period of time determined by an explicit
;; distribution. This means we'll need a poisson distribution
;; for arrivals, and a separate distribution for durations of
;; stay.

(define ARRIVAL-RATE 1/15) ;; arrivals per minute

;; for a living room?
#;(define arrival-dist (poisson-dist ARRIVAL-RATE))
#;(define duration-dist (discrete-dist (list 0 2 (* 5 60) (* 10 60) (* 30 60)
                                           (* 120 60))
                                     (list 1 3 20 5 40 40)))

;; for a bathroom?
(define arrival-dist (poisson-dist 1/60))
(define duration-dist (discrete-dist (list 0 2 (* 5 60))
                                     (list 1 3 20)))

;; a runnning state is characterized by the length of time until the last
;; person leaves, or #f if no one is in the room.

;; we sample the arrival distribution every minute.

;; a transition is either 'turn-on or 'turn-off


(define (bogo-sleep n)
  (sleep 0.5))

(define (light-on!)
  (printf "turning light on\n"))

(define (light-off!)
  (printf "turning light off\n"))


(random-seed 33998)

(let loop ([state #f] [timeofday (* 8 3600)])
  (printf "time: ~a:~a\n" (floor (/ timeofday 3600))
          (floor (/ (modulo timeofday 3600) 60)))
  (define daytime? (< (* 7 3600) timeofday (* 22 3600)))
  (define arrivals (cond [daytime? (inexact->exact (sample arrival-dist))]
                         ;; no one comes in at night:
                         [else 0]))
  (define longest-stayer
    (and (< 0 arrivals)
         (apply max (for/list ([i arrivals]) (sample duration-dist)))))
  (when longest-stayer (printf "arrivals! longest for ~v seconds\n" longest-stayer))
  (define new-state (cond [(and state longest-stayer)
                           (max state longest-stayer)]
                          [else (or state longest-stayer)]))
  (when (and (not state) new-state)
    ;; someone turned the light on!
    (light-on!))
  (cond [(and new-state (<= new-state 60))
         ;; leaving the room before the next arrival opportunity:
         (bogo-sleep new-state)
         (light-off!)
         (bogo-sleep (- 60 new-state))
         (loop #f (modulo (+ timeofday 60) (* 24 3600)))]
        [else
         (bogo-sleep 60)
         (loop (and new-state (- new-state 60)) (modulo (+ timeofday 60) (* 24 3600)))]))


