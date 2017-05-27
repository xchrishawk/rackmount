;;
;; log.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Module containing utility functions for logging.
;;

#lang racket

;; -- Requires --

(require racket/date)

;; -- Provides --

(provide
 (contract-out

  ;; Logs an event to the main server log.
  [rackmount-log (->* (string? (or/c string? false?) string?) #:rest (listof any/c) void?)]

  ;; Creates a local logging procedure for the specified category.
  [create-local-log (-> string? (->* (string?) #:rest (listof any/c) void?))]

  ;; Creates a local logging procedure for the specified category/identifier.
  [create-local-log/identifier (-> string? (->* (any/c string?) #:rest (listof any/c) void?))]))

;; -- Public Procedures --

(define (rackmount-log category identifier fmt . v)
  (let* ([category/identifier (if identifier
                                  (format "~A / ~A" category identifier)
                                  category)]
         [date (current-date)]
         [milliseconds (exact-floor (/ (date*-nanosecond date) 1000000.0))]
         [event (apply format fmt v)])
    ;; For now, just print to console
    (displayln (format "[ ~A:~A:~A.~A | ~A ] ~A"
                       (padded-number-string (date-hour date) 2)
                       (padded-number-string (date-minute date) 2)
                       (padded-number-string (date-second date) 2)
                       (padded-number-string milliseconds 3)
                       category/identifier
                       event))))

(define (create-local-log category)
  (λ (fmt . v)
    (apply rackmount-log category #f fmt v)))

(define (create-local-log/identifier category)
  (λ (identifier fmt . v)
    (apply rackmount-log category identifier fmt v)))

;; -- Private Procedures --

;; Returns a padded number string.
(define (padded-number-string value width)
  (~a value #:width width #:align 'right #:pad-string "0"))
