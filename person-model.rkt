#lang racket

(require "generate-sensor-names.rkt")
;; this will be a model of a person.

(require rackunit
         racket/stream)

;; a mapping from hour and minute to seconds
;(: t (Natural Natural -> Natural))
(define (t h m)
  (+ (* h HOUR-SECONDS)
     (* m MINUTE-SECONDS)))

(define DAY-SECONDS 86400)
(define HOUR-SECONDS 3600)
(define MINUTE-SECONDS 60)

(check-equal? (t 8 19)
              (+ (* 8 3600) (* 19 60)))

;; a simple schedule-based model for lighting.
;; a schedule is a list of event-begins. An event
;; is presumed to continue until the beginning
;; of the next event

;; one key assumption of this model is that each
;; person's schedule is independent; if we want
;; dependencies, we'll have to formulate a different
;; model.

;; an event-begin is a list containing an activity
;; name and a number of seconds (since midnight)
;(define-type schedule (List Natural Symbol))
(define example-kid-schedule
  `((,(t 7 00) home)
    (,(t 7 15) breakfast)
    (,(t 7 30) school)
    (,(t 3 30) home)
    (,(t 6 00) dinner)
    (,(t 7 00) home)
    (,(t 8 30) sleep)))

;; initially, each event-begin generates
;; a single room-move-event

;; an event is (make-event seconds person lights)
;; where seconds is a natural number in 0 .. 86400,
;; person is a symbol, and lights is a list of symbols.
(struct event (seconds person lights) #:prefab)

;(define-type Event (List Natural Symbol (Listof Symbol)))

;(: example-event Event)
(define example-event (event (t 9 45) 'alexander '(s-light-chandelier-1B)))


;; given an activity, return a list of lights on for that activity
;(: light-map (Listof (List Symbol (Listof Symbol))))
(define light-map
  `((breakfast (s-light-kitchen-uplight-3A
                s-light-pendant-bar-lights-3C))
    (school ())
    (home (s-light-flexspace-uplight-5A
           s-light-tv-light-2A))
    (dinner (s-light-chandelier-1B))
    (sleep ())))

;; return the set of lights associated with an activity
;(: activity-lights (Symbol Symbol -> (Setof Symbol)))
(define (activity-lights name activity)
  (match (dict-ref light-map activity)
    [#f (error 'activity-lights "no match for activity named ~e" activity)]
    [(list lights) (list->set lights)]
    [other (error 'activity-lights "too many matches for activity named ~e" activity)]))


;; an elist is a stream of events
;(define-type EList (Stream Event))
(define (schedule->elist name schedule)
  ;(: generator (Schedule -> EList))
  (define (generator sched)
    (cond [(empty? sched) (generator schedule)]
          [else (match-define (list time activity) (first sched))
                (stream-cons (event time name (activity-lights name activity))
                             (generator (rest sched)))]))
  (generator schedule))


;; merges two ELists. Assumes it's starting at midnight!
;(: elist-merge (EList EList -> EList))
(define (elist-merge a b)
  (let loop ([cur-time 0] [a a] [b b])
    (define a-fst (stream-first a))
    (define seconds-til-a (modulo (- (event-seconds a-fst) cur-time)
                                  DAY-SECONDS))
    (define b-fst (stream-first b))
    (define seconds-til-b (modulo (- (event-seconds b-fst) cur-time)
                                  DAY-SECONDS))
    (cond [(< seconds-til-a seconds-til-b)
           (stream-cons a-fst (loop (event-seconds a-fst)
                                    (stream-rest a)
                                    b))]
          [else
           (stream-cons b-fst (loop (event-seconds b-fst)
                                    a
                                    (stream-rest b)))])))

;; a house-state is a mapping from person to what lights they're
;; using. Specifically, it's a hash table.

;; given a house-state and an event, produce a new house-state
;; and a list of the light-on-off messages.
(define (update-house-state house-state evt)
  (define previously-on (lights-on house-state))
  (define new-state
    (hash-set house-state (event-person evt) (event-lights evt)))
  (define now-on (lights-on new-state))
  (define turned-on (set-subtract now-on previously-on))
  (define turned-off (set-subtract previously-on now-on))
  (list new-state turned-on turned-off))

;; given a house-state, return the lights that are on
(define (lights-on house-state)
  (apply set-union (map list->set (hash-values house-state))))

;; given an elist and an initial house-state, generate
;; a lazy stream of light-on and light-off messages
(define (house-stream elist state)
  (define first-event (stream-first elist))
  (match-define (list new-state now-on now-off)
    (update-house-state state first-event))
  (define on-events
    (for/list ([l (in-set now-on)])
      (list (event-seconds first-event) l 'on)))
  (define off-events
    (for/list ([l (in-set now-off)])
      (list (event-seconds first-event) l 'off)))
  (my-stream-append
   (append on-events off-events)
   (lambda () (house-stream (stream-rest elist) new-state))))

;; given a list, produce a stream
(define (my-stream-append l thunk)
  (cond [(empty? l) (thunk)]
        [else (stream-cons (first l) (my-stream-append (rest l) thunk))]))

;; TESTS 

(check-equal? (update-house-state (hash 'alex (set 'l1 'l2) 'bobby (set 'l2 'l4))
                                  (event 3879 'bobby (set 'l1 'l9)))
              (list (hash 'alex (set 'l1 'l2) 'bobby (set 'l1 'l9))
                    (set 'l9)
                    (set 'l4)))

;; it's really weird that stream-take isn't there. Also no for/stream.
;; Also no Stream TR type. Also no list->stream. Am I missing something
;; obvious?

(define (stream-take stream n)
  (for/list ([elt (in-stream stream)]
             [i (in-range n)])
    elt))

(define test-schedule
  `((,(t 5 00) breakfast)
    (,(t 5 10) sleep)))

(define freddy-estream (schedule->elist 'freddy test-schedule))
(check-equal? (stream-take freddy-estream 3)
              (list (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))
                    (event (t 5 10) 'freddy (set))
                    (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))))

(define joey-schedule
  `((,(t 5 04) breakfast)
    (,(t 5 09) sleep)))
(define joey-estream (schedule->elist 'joey joey-schedule))
(check-equal? (stream-take (elist-merge freddy-estream
                                        joey-estream)
                           5)
              (list (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))
                    (event (t 5 04) 'joey (set 's-light-kitchen-uplight-3A
                                               's-light-pendant-bar-lights-3C))
                    (event (t 5 09) 'joey (set))
                    (event (t 5 10) 'freddy (set))
                    (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))))
;; reverse the order of the stream arguments:
(check-equal? (stream-take (elist-merge joey-estream
                                        freddy-estream)
                           5)
              (list (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))
                    (event (t 5 04) 'joey (set 's-light-kitchen-uplight-3A
                                               's-light-pendant-bar-lights-3C))
                    (event (t 5 09) 'joey (set))
                    (event (t 5 10) 'freddy (set))
                    (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))))


(define test-elist (elist-merge joey-estream freddy-estream))

(house-stream test-elist (hash 'joey (set) 'freddy (set)))

(check-equal? (stream-take (house-stream (elist-merge joey-estream freddy-estream)
                                         (hash 'joey (set)
                                               'freddy (set)))
                           5)
              (list (list (t 5 00) 's-light-kitchen-uplight-3A 'on)
                    (list (t 5 00) 's-light-pendant-bar-lights-3C 'on)
                    (list (t 5 10) 's-light-kitchen-uplight-3A 'off)
                    (list (t 5 10) 's-light-pendant-bar-lights-3C 'off)
                    (list (t 5 00) 's-light-kitchen-uplight-3A 'on)))



;; I think we'll model a person as a priority queue of processes. Processes
;; can change their priorities over time. There will be a "running" process
;; that will control where the person goes.

#;(
(define bathroom-process
  (sequence (go-to 'bathroom)
            (stay-for (approximately (minutes 5)))))

(define bathroom-priority
  (cond [(running? bathroom-process) 100]
        [else (define t (time-since-process-completed bathroom-process))
              (cond [(< t 30) 0]
                    [(< t 60) 5]
                    [(< t 90) 10]
                    [(< t 120) 20]
                    [(< t 180) 40]
                    [else 80])]))

;; go to a particular room
(define (go-to room)
  (printf "going to ~v\n" room))

;; work on a process for some number of seconds.
(define (stay-for seconds)
  (sleep seconds)))

