#lang racket

;; -- Requires --

(require racket/date)

;; -- Provides --

(provide
 (contract-out
  [rackmount-log (->* (string? string?) #:rest (listof any/c) void?)]
  [create-local-log (-> string? (->* (string?) #:rest (listof any/c) void?))]))

;; -- Public Procedures --

;; Logs an event to the main log.
(define (rackmount-log category fmt . v)
  (let* ([date (current-date)]
         [milliseconds (exact-floor (/ (date*-nanosecond date) 1000000.0))]
         [event (apply format fmt v)])
    ;; For now, just print to console
    (displayln (format "[~A|~A:~A:~A.~A] ~A"
                       category
                       (padded-number-string (date-hour date) 2)
                       (padded-number-string (date-minute date) 2)
                       (padded-number-string (date-second date) 2)
                       (padded-number-string milliseconds 3)
                       event))))

;; Returns a lambda for logging events with the specified category.
(define (create-local-log category)
  (λ (fmt . v)
    (apply rackmount-log category fmt v)))

;; -- Private Procedures --

(define (padded-number-string value width)
  (~a value #:width width #:align 'right #:pad-string "0"))
