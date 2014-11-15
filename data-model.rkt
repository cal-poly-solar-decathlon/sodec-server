#lang racket

;; okay, just thinking out loud about devices

;; a device is (make-device id kind)
(struct device (id kind))

;; a kind is one of
;; - 'temperature-sensor
;; - 'occupancy-sensor
;; - 'ambient-light-sensor
;; - 'ammeter
;; - 'dimmable-light-control
;; - 'on-off-light-control

