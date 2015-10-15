#lang racket

;; this file is just for interactive querying--it's easier than constructing URLs by hand.
(require net/url
         net/head
         racket/date
         racket/block
         racket/contract
         racket/format
         json
         "../device-table.rkt"
         "../web-funs.rkt")

(define-logger sodec)

(module* test racket/base)

(define HOST 
  ;; test locally:
  #;"localhost"
  #;"129.65.138.226"
  "calpolysolardecathlon.org"
  #;"192.168.2.3")

(define PORT
  3000
  #;8080)

(define (gett . args)
  in-list (remote-call/get HOST PORT (apply sodec-url args)))

(define (get-timestamp)
  (hash-ref (gett "timestamp" #f) 'timestamp))

   ;; expected intervals, in seconds
   (define TEMP-HUM-INTERVAL 45)
   (define ELEC-INTERVAL 15)


#;(block
   
   ;; status report covers this many trailing seconds
   (define CHECK-TIME (* 60 60 24))
   
   (define (check-time-events-count measurement device)
     (define ts (current-seconds))
     (gett "count-events-in-range" `((measurement ,measurement)
                                     (device ,device)
                                     (start ,(- ts CHECK-TIME))
                                     (end ,ts))))
   
   (define temp-hum-readings-expected (/ CHECK-TIME TEMP-HUM-INTERVAL))
   (define elec-readings-expected (/ CHECK-TIME ELEC-INTERVAL))
   
   (define (print-status-report)
     (for ([measurement (in-list MEASUREMENT-NAMES)])
       (printf "## ~a\n" measurement)
       (define expected-num-readings
         (cond [(equal? measurement "electric_power") elec-readings-expected]
               [else temp-hum-readings-expected]))
       (for ([device (hash-ref measurement-device-table measurement)]
             #:when (not (regexp-match #px"^testing_" device)))
         (define num-readings (check-time-events-count measurement device))
         (define pct (round
                      (* 100 (/ num-readings expected-num-readings))))
         (define alert (cond [(<= pct 95)
                              (format " : *LOW* ~a%" pct)]
                             [(<= pct 100) ""]
                             [else
                              (format " : *HIGH* ~a%" pct)]))
         (printf "~v : ~v~a\n"
                 device
                 num-readings
                 alert))))
   
   (print-status-report))


;; convert watt-seconds to watt-hours
(define (ws->wh ws)
  (/ ws 3600))

(define all-electric-devices (hash-ref measurement-device-table "electric_power"))

(define START (find-seconds 0 0 0 13 10 2015))
(define END   (find-seconds 0 0 0 14 10 2015))
(define all-device-usages
  (for/list ([device all-electric-devices]
             #:when (not (regexp-match #px"^testing_" device)))
    ;; ouch, double the air_conditioning:
    (define multiplier
      (cond [(equal? device "air_conditioning") 2]
            [else 1]))
    (list
     device
     (round
      (* multiplier
         (ws->wh
          (-
           (hash-ref
            (gett "interval-first-event" `((measurement "electric_power")
                                           (device ,device)
                                           (start ,START)
                                           (end ,END)))
            'r)
           (gett "interval-last-event" `((measurement "electric_power")
                                         (device ,device)
                                         (start ,START)
                                         (end ,END))))))))))

(define generation-devices
  '(("main_solar_array" -1)
    ("bifacial_solar_array" -1)
    ;; CT on mains installed backward...
    ("mains" 1)))

(define total-used
  (apply +
         (for/list ([device (in-list generation-devices)])
           (* (cadr device)
              (car (dict-ref all-device-usages (first device)))))))

(define budget-map
  '(("refrigerator" ("refrigerator"))
    ("stovetop" ("induction_stove"))
    ("cooking_task" ())
    ("dishwasher" ("dishwasher"))
    ("oven" ("microwave"))
    ("washing machine" ("laundry"))
    ("HVAC" ("air_conditioning" "air_handler" "heat_recovery_ventilation"))
    ("Passive HVAC inline Fan" ())
    ("Mechanical Room Fan" ())
    ("blackwater pump" ("blackwater_pump"))
    ("greywater pump" ("greywater_pump"))
    ("lighting (assume 400 W)" ("lighting_1" "lighting_2"))
    ("Laptop" ())
    ("TV" ())
    ("controls system" ("mechanical_room_outlets"))
    ("vehicle" ("vehicle_charging_station"))
    ("everything_else" ("everything_else"))
    ("water_heater" ("water_heater" "thermal_loop_pump"))
    ("UNCHARGED" ("water_supply_pump"))))

(define budgeted-devices
  (apply append (map second budget-map)))

(unless (set-empty?
         (set-intersect (list->set budgeted-devices)
                        (list->set (map first generation-devices))))
  (error 'set-empty "intersection of generation and budgeted not empty"))

(unless (equal?
         (set-union (set "everything_else")
                    (list->set all-electric-devices))
         (set-union (list->set budgeted-devices)
                    (list->set (map first generation-devices))))
  (printf "all - union: ~v" (set-subtract (list->set all-electric-devices)
                                        (set-union (list->set budgeted-devices)
                                                   (list->set (map first generation-devices)))))
  (printf "union - all: ~v" (set-subtract (set-union (list->set budgeted-devices)
                                                   (list->set (map first generation-devices)))
                                        (list->set all-electric-devices)))
  (error 'devices ""))

(define total-known
  (apply +
         (for/list ([usage (in-list all-device-usages)]
                    #:when (not (member (car usage) (map first generation-devices))))
           (second usage))))

(define everything-else (- total-used total-known))

(define all-usages-with-everything-else
  (cons (list "everything_else" everything-else)
        all-device-usages))

(for/list ([b (in-list budget-map)])
  (exact->inexact
   (/ (apply
       +
       (for/list ([device (in-list (second b))])
         (first (dict-ref all-usages-with-everything-else device))))
      1000)))

(printf "total used: ~v\n" total-used)


#;(block
 (define ts (current-seconds))
 (define (check-hour-events-count measurement device hours-ago)
   (gett "count-events-in-range" `((measurement ,measurement)
                                   (device ,device)
                                   (start ,(- ts (* hours-ago 3600)))
                                   (end ,(- ts (* (sub1 hours-ago) 3600))))))
 
 (define temp-hum-readings-expected (/ 3600 TEMP-HUM-INTERVAL))
 (define elec-readings-expected (/ 3600 ELEC-INTERVAL))
 
 (define (print-hour-line hours-ago)
   (for ([measurement (in-list MEASUREMENT-NAMES)])
     (define expected-num-readings
       (cond [(equal? measurement "electric_power") elec-readings-expected]
             [else temp-hum-readings-expected]))
     (for ([device (hash-ref measurement-device-table measurement)]
           #:when (not (regexp-match #px"^testing_" device)))
       (define num-readings (check-hour-events-count measurement device hours-ago))
       (define pct (round
                    (* 100 (/ num-readings expected-num-readings))))
       (define status-char
         (cond [(<= pct 2) "X"]
               [(<= pct 75) "d"]
               [(<= pct 80)  "c"]
               [(<= pct 90) "b"]
               [(<= pct 95) "a"]
               [else "."]))
       (display status-char)))
   (display "\n"))

 (define LAST-HOURS 24)
 (for ([i LAST-HOURS])
   (print-hour-line (- LAST-HOURS i))))



;; ping check returns wrong result
;; no events wrong in both places

(define (string-or-null? s)
  (or (eq? s 'null)
      (string? s)))


(define (time-near-now? n)
  (and (exact-integer? n)
       (< (abs (- (current-seconds) n)) 100)))

(define ((port-containing str) port)
  (regexp-match (regexp-quote str) port))

(define ((eqto? a) b)
  (equal? a b))



