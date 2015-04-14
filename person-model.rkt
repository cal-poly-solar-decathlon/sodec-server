#lang racket

(require "send-reading.rkt"
         rackunit
         racket/stream
         racket/date)

;; this will be a model of a person, and how they
;; turn lights on and off.

(provide run-alice-barry-lights)

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
    (,(t 15 30) home)
    (,(t 18 00) dinner)
    (,(t 19 00) home)
    (,(t 20 30) sleep)))

;; initially, each event-begin generates
;; a single room-move-event

;; an event is (make-event seconds person lights)
;; where seconds is a natural number in 0 .. DAY-SECONDS,
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
    (work ())
    (sleep ())
    (home (s-light-flexspace-uplight-5A
           s-light-tv-light-2A))
    (make-dinner (s-light-kitchen-uplight-3A
                  s-light-under-counter-3B
                  s-light-pendant-bar-lights-3C))
    (eat-dinner (s-light-chandelier-1B))))

;; return the set of lights associated with an activity
;(: activity-lights (Symbol Symbol -> (Setof Symbol)))
(define (activity-lights name activity)
  (match (dict-ref light-map activity)
    [#f (error 'activity-lights "no match for activity named ~e" activity)]
    [(list lights) (list->set lights)]
    [other (error 'activity-lights "too many matches for activity named ~e" activity)]))


;; an elist is a stream of events
;(define-type EList (Stream Event))

;; note that for a stream of events, it may be totally unclear
;; when a day has elapsed; for instance, if you have 12 events
;; in a row with time 5:00, does that mean it happens once per
;; day, or that all twelve happen at once?
;;
;; we're going to assume the latter, and check that every
;; schedule has more than one time in it.

(define (schedule->elist name schedule)
  (check-schedule schedule)
  ;(: generator (Schedule -> EList))
  (define (generator sched)
    (cond [(empty? sched) (generator schedule)]
          [else (match-define (list time activity) (first sched))
                (stream-cons (event time name (activity-lights name activity))
                             (generator (rest sched)))]))
  (generator schedule))

;; we imagine that sending a reading could take this long. If the
;; next event is less than this many seconds in the past, we assume
;; that it's just one we're late in delivering, and we send it now.
;; if it's older than this, we assume that it's supposed to happen
;; tomorrow.

;; if this number is longer than 6 hours, things get bad with check-schedule,
;; but that seems ... unlikely.
(define MAX-OOPS (* 5 60))


;; check that a schedule covers more than MAX-OOPS.
;; actually, this is kind of surprisingly painful; in order to deal
;; correctly with wrap-around, you have to try dividing at midnight and
;; at noon.
(define (check-schedule schedule)
  (define times (remove-duplicates (map first schedule)))
  (define cover-1 (- (apply max times) (apply min times)))
  (define times-12 (for/list ([t times]) (modulo (+ t (* 12 HOUR-SECONDS))
                                                 DAY-SECONDS)))
  (define cover-2 (- (apply max times-12) (apply min times-12)))
  (unless (and (< MAX-OOPS cover-1)
               (< MAX-OOPS cover-2))
    (raise-argument-error 'check-schedule
                          (format "schedule spanning more than ~v seconds"
                                  MAX-OOPS)
                          0 schedule)))



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

;; find the number of seconds since midnight:
(define (seconds-since-midnight)
  (define current-date-seconds (current-seconds))
  (define current-date (seconds->date current-date-seconds))
  (define midnight (find-seconds 0 0 0 (date-day current-date)
                                 (date-month current-date)
                                 (date-year current-date)))
  (- current-date-seconds midnight))

;; given a house-stream and a host, issue readings
(define (run-house-stream house-stream)
  (define synced-stream (discard-events-before (seconds-since-midnight)
                                               house-stream))
  ;; this is going to slip, but I don't think it really matters...
  (let loop ([house-stream synced-stream]
             [now-time (seconds-since-midnight)])
    (log-sodec-debug "next event: ~v" (stream-first house-stream))
    (match-define (list time light state) (stream-first house-stream))
    (define light-level (match state
                          ['on 100]
                          ['off 0]))
    (cond [(or (<= (- now-time MAX-OOPS) time now-time)
               (<= (- now-time MAX-OOPS) (- time DAY-SECONDS) now-time))
           (send-reading! (symbol->string light) light-level)
           (loop (rest house-stream) now-time)]
          [else
           (log-sodec-debug "sleeping for ~v seconds until ~v\n"
                            (- time now-time) time)
           (sleep (modulo (- time now-time) DAY-SECONDS))
           (loop house-stream (seconds-since-midnight))])))

(define (log-sodec-debug . args)
  (log-message (current-logger)
               'debug
               'sodec
               (apply format args)))


;; discard the events that are between midnight and the given time
(define (discard-events-before time house-stream)
  (cond [(< time (first (stream-first house-stream)))
         house-stream]
        [(< (first (stream-first (stream-rest house-stream)))
            (first (stream-first house-stream)))
         house-stream]
        [else (discard-events-before time (stream-rest house-stream))]))

;; let's actually try it...

(define alice-schedule
  `((,(t 7 00) home)
    (,(t 7 15) breakfast)
    (,(t 8 00) work)
    (,(t 16 30) home)
    (,(t 18 00) eat-dinner)
    (,(t 19 00) home)
    (,(t 21 30) sleep)))

(define barry-schedule
  `((,(t 7 05) home)
    (,(t 7 11) breakfast)
    (,(t 7 30) home)
    (,(t 8 45) work)
    (,(t 16 45) home)
    (,(t 17 00) make-dinner)
    (,(t 18 00) eat-dinner)
    (,(t 19 00) home)
    (,(t 22 00) sleep)))

(check-not-exn
 (lambda ()
   (map (lambda (activity) (activity-lights 'dannybogus activity))
        (map second (append barry-schedule alice-schedule)))))

(define (run-alice-barry-lights)
  (thread
   (lambda ()
     (run-house-stream
      (house-stream
       (elist-merge
        (schedule->elist 'alice alice-schedule)
        (schedule->elist 'barry barry-schedule))
       (hash 'alice (set)
             'barry (set)))))))


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
  `((,(t 5 03) breakfast)
    (,(t 5 09) sleep)))
(define joey-estream (schedule->elist 'joey joey-schedule))
(check-equal? (stream-take (elist-merge freddy-estream
                                        joey-estream)
                           5)
              (list (event (t 5 00) 'freddy (set 's-light-kitchen-uplight-3A
                                                 's-light-pendant-bar-lights-3C))
                    (event (t 5 03) 'joey (set 's-light-kitchen-uplight-3A
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
                    (event (t 5 03) 'joey (set 's-light-kitchen-uplight-3A
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

(define test-house-stream
  (let loop () (stream-cons (list (t 5 00) 'foo)
                            (stream-cons (list (t 5 01) 'bar)
                                         (loop)))))
(check-pred (lambda (x) #t) (discard-events-before (t 6 00) test-house-stream))

(check-exn (lambda (exn)
             (regexp-match #px"schedule spanning more than"
                           (exn-message exn)))
           (lambda () (check-schedule `((,(t 5 00) foo) (,(t 5 02) bar)))))

(check-exn (lambda (exn)
             (regexp-match #px"schedule spanning more than"
                           (exn-message exn)))
           (lambda () (check-schedule `((,(t 0 01) foo) (,(t 23 59) bar)))))

;; TOO COMPLICATED:

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

