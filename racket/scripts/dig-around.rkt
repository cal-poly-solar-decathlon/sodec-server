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

(block
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

 (define LAST-HOURS 5)
 (for ([i LAST-HOURS])
   (print-hour-line (- LAST-HOURS i))))


;; convert watt-seconds to watt-hours
(define (ws->wh ws)
  (/ ws 3600))


(define all-device-usages
  (for/list ([device (hash-ref measurement-device-table "electric_power")]
             #:when (not (regexp-match #px"^testing_" device)))
    (define ts (find-seconds 0 0 0 9 10 2015))
    (define contest-start (find-seconds 0 0 11 8 10 2015))
    (list
     device
     (round
      (ws->wh
       (-
        (hash-ref
         (gett "interval-first-event" `((measurement "electric_power")
                                        (device ,device)
                                        (start ,contest-start)
                                        (end ,ts)))
         'r)
        (gett "interval-last-event" `((measurement "electric_power")
                                      (device ,device)
                                      (start ,contest-start)
                                      (end ,ts)))))))))

(define generation-devices
  '(("main_solar_array" -1)
    ("bifacial_solar_array" -1)
    ;; CT on mains installed backward...
    ("mains" 1)))

(define total-used
  (for/list ([device (in-list generation-devices)])
    (car (dict-ref all-device-usages (first device)))))

#;(define total-used
  (apply +
         (map cadr
              '(("main_solar_array" -9365)
                ("bifacial_solar_array" -17203)
                ("mains" 16045)))))

(define known-used
  (apply +
         (map cadr '(("laundry" 9)
    ("dishwasher" 3)
    ("refrigerator" 912)
    ("induction_stove" 1682)
    ("water_heater" -6)
    ("greywater_pump" 17)
    ("blackwater_pump" 18)
    ("thermal_loop_pump" 333)
    ("water_supply_pump" 78)
    ("vehicle_charging_station" 13)
    ("mechanical_room_outlets" 3)
    ("heat_recovery_ventilation" 1)
    ("air_handler" 484)
    ("air_conditioning" 2188)
    ("microwave" 603)
    ("lighting_1" 727)
    ("lighting_2" 772)))))

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



