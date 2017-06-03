;;
;; conversion.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Converts seconds to milliseconds.
  [seconds->milliseconds (-> real? real?)]

  ;; Converts milliseconds to seconds.
  [milliseconds->seconds (-> real? real?)]))

;; -- Procedures --

(define (seconds->milliseconds seconds)
  (* seconds 1000.0))

(define (milliseconds->seconds milliseconds)
  (/ milliseconds 1000.0))
