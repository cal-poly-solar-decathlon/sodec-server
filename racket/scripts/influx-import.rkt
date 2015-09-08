#lang racket

(require net/url
         rackunit
         racket/date)

(define SQLDUMP "/Users/clements/sodecdump.sql")
(define measurements
  '(temperature humidity electricity_used
                electricity_generated
                light_level
                bogus))
;; just an estimate:
(define TOTAL-RECORDS 8000000)

(define port (open-input-file SQLDUMP))
(define output-port-table
  (for/list ([m measurements])
    (define filename (~a "/tmp/influx-"m".txt"))
    (define port (open-output-file filename #:exists 'truncate))
    (list m port)))

(define temp-lookup
  '((lr living_room)
    (bed bedroom)
    (kit kitchen)
    (out outside)
    (bath bathroom)
    (testing-blackhole testing_blackhole)))

;; given an existing device name, return measurement and new device name
(define (parse-device-name name)
  (match (regexp-split #px"-" name)
    [(list-rest #"s" #"temp" temp-name-list)
     (define temp-name
       (string->symbol
        (apply string-append
               (add-between (map bytes->string/utf-8 temp-name-list) "-"))))
     (match (assoc temp-name
                   temp-lookup)
       [(list dc new-name)
        (list "temperature" (symbol->string new-name))]
       [#f (error 'parse-device-name
                  "no match in table for ~v" temp-name)])]
    [(list #"s" #"hum" hum-name)
     (match (assoc (string->symbol
                    (bytes->string/utf-8
                     hum-name))
                   temp-lookup)
       [(list dc new-name)
        (list "humidity" (symbol->string new-name))]
       [#f (error 'parse-device-name
                  "no match in table for ~v" hum-name)])]
    [(list #"c" #"light" light-name)
     (list "bogus" light-name)]
    [other
     (error 'parse-device-name
            "giving up on ~v" other)]))

(check-equal? (parse-device-name #"s-temp-lr")
              (list "temperature" "living_room"))
(check-equal? (parse-device-name #"s-temp-testing-blackhole")
              (list "temperature" "testing_blackhole"))

;; convert a sql date to a number of seconds
;; IGNORING TIME ZONE ISSUES
(define (sql-date->seconds str)
  (match (regexp-match
          #px"^([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2}) ([[:digit:]]{2}):([[:digit:]]{2}):([[:digit:]]{2})$"
          str)
    [#f (raise-argument-error 'sql-date->seconds
                              "well-formatted date"
                              0 str)]
    [fields (apply find-seconds
                   (map string->number
                        (map bytes->string/utf-8
                             (reverse (cdr fields)))))]))

(check-equal? (sql-date->seconds #"2015-05-11 05:08:25")
              (find-seconds 25 8 5 11 5 2015))

(regexp-match (regexp-quote #"INSERT INTO `sensorevents` VALUES ")
              port)

(define RECORD-REGEXP
  #px"^\\(([[:digit:]]+),'([^']*)','([^']*)',(-?[[:digit:]]+)\\)")
(define BETWEEN-RECORDS-REGEXP
  #px"^(,|;\nINSERT INTO `sensorevents` VALUES )")

(define start-time (current-seconds))


;; ignore sequential device readings that occur within this many seconds:
(define MIN-INTERVAL 60)

(define last-reading (make-hash))

(let loop ([last-index #f])
  (when (and (number? last-index)
             (= (modulo last-index 10000) 0))
    (printf "processing record # ~a\n" last-index)
    (define time-taken (- (current-seconds)
                          start-time))
    (printf "time taken so far: ~a seconds\n" time-taken)
    (define estimated-total
      (* (/ TOTAL-RECORDS last-index)
         time-taken))
    (printf "estimated time remaining: ~a seconds\n"
            (round (- estimated-total time-taken))))
  (match (regexp-match RECORD-REGEXP port)
    [#f (list 'done-1 last-index)]
    [(list dc index sensor-name date reading)
     (define date-sec (sql-date->seconds date))
     (define last-device-reading (hash-ref last-reading sensor-name #f))
     (cond [(or (not last-device-reading) (> (- date-sec last-device-reading) MIN-INTERVAL))
            (match (parse-device-name sensor-name)
              [(list measurement device)
               (fprintf
                (match (assoc
                        (string->symbol
                         measurement)
                        output-port-table)
                  [#f (error 'parsing "no match for measurement ~v"
                             measurement)]
                  [other (cadr other)])
                "~a,device=~a reading=~ai ~a\n"
                measurement
                device
                reading
                date-sec)])
            (hash-set! last-reading sensor-name date-sec)]
           [else ;; not long enough since last reading, ignore it
            last-reading])
     (match (regexp-match BETWEEN-RECORDS-REGEXP port)
       [#f (list 'done-2 last-index)]
       [other (loop (string->number (bytes->string/utf-8 index)))])]))


;(1,'s-temp-lr','2015-05-11 05:08:25',0)
(close-input-port port)
(for ([port (in-list (map cadr output-port-table))])
  (close-output-port port))


#;(http-sendrecv/url
 (string->url "http://localhost:8086/write?db=mydb")
 #:method #"POST"
 #:data #"cpu_load_shortaa,host=server01,region=us-west value=0.64
cpu_load_shortaa,host=server02,region=us-west value=0.55 1422568543702900257
cpu_load_shortaa,direction=in,host=server01,region=us-west value=23422.0 1422568543702900257")

;;&precision=s

;; import command:
;; curl -i 'http://localhost:8086/write?db=mydb&precision=s' --data-binary @/tmp/influx-temperature.txt
