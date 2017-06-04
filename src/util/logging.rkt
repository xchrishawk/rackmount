;;
;; logging.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines shared types and procedures for logging.
;;

#lang racket

;; -- Requires --

(require (for-syntax syntax/parse))
(require (for-syntax "../util/syntax.rkt"))

;; -- Provides --

(provide

 ;; Macro which defines a det of functions for logging for the local module.
 define-local-log

 (contract-out

  ;; Struct representing an event to be logged.
  [struct log-event ([date inexact?]
                     [level log-event-level?]
                     [category string?]
                     [identifier (or/c string? false?)]
                     [text string?])]

  ;; Reports a log event.
  [log-event-report (-> log-event? void?)]

  ;; Returns a formatted string for the specified log event.
  [log-event->string (-> log-event? string?)]

  ;; Predicate returning #t if the argument is a valid log level.
  [log-event-level? (-> any/c boolean?)]

  ;; Returns #t if the specified log event level is enabled, based on the
  ;; specified minimum log event level.
  [log-event-level-enabled? (-> log-event-level? log-event-level? boolean?)]))

;; -- Structs --

(struct log-event (date		; the date the event occurred, in milliseconds
                   level	; the level of the event
                   category	; the category to file the event
                   identifier	; the identifier of the object which raised the event
                   text)	; the text of the event
  #:transparent)

;; -- Objects --

;; Lookup table for log levels.
(define log-event-levels (hash 'critical (cons 0 "Critical")
                               'error (cons 1 "Error")
                               'warning (cons 2 "Warning")
                               'info (cons 3 "Info")
                               'debug (cons 4 "Debug")
                               'trace (cons 5 "Trace")))

;; -- Public Procedures --

(define (log-event-report log-event)
  (displayln (log-event->string log-event))
  (newline))

(define (log-event->string log-event)
  (let* ([time (inexact-milliseconds->string (log-event-date log-event))]
         [level (log-event-level->string (log-event-level log-event))]
         [category (log-event-category log-event)]
         [identifier (log-event-identifier log-event)]
         [header (if identifier
                     (format "~A / ~A / ~A / ~A" time level category identifier)
                     (format "~A / ~A / ~A" time level category))])
    (format "[~A]\n~A" header (log-event-text log-event))))

(define (log-event-level? x)
  (if (hash-ref log-event-levels x #f) #t #f))

(define (log-event-level-enabled? log-event-level minimum-log-event-level)
  (<= (log-event-level->integer log-event-level)
      (log-event-level->integer minimum-log-event-level)))

;; -- Private Procedures --

;; Convert a log level symbol to an integer.
(define (log-event-level->integer log-event-level)
  (car (hash-ref log-event-levels log-event-level)))

;; Convert a log level symbol to a string.
(define (log-event-level->string log-event-level)
  (cdr (hash-ref log-event-levels log-event-level)))

;; Returns a date string for the specified date. The date should be a value returned
;; from current-seconds (i.e., a time period past the UNIX epoch).
(define (inexact-milliseconds->string inexact-milliseconds)
  (define (padded-number-string value width)
    (~a value #:width width #:align 'right #:pad-string "0"))
  (let* ([date (seconds->date (/ inexact-milliseconds 1000.0))])
    (format "~A-~A-~A ~A:~A:~A.~A"
            (padded-number-string (date-year date) 4)
            (padded-number-string (date-month date) 2)
            (padded-number-string (date-day date) 2)
            (padded-number-string (date-hour date) 2)
            (padded-number-string (date-minute date) 2)
            (padded-number-string (date-second date) 2)
            (padded-number-string (exact-floor (/ (date*-nanosecond date) 1000000.0)) 3))))

;; -- Macros --

(define-syntax (define-local-log stx)
  (syntax-parse stx
    [(_ name:id category:str (~optional (~seq #:with-identifier with-identifier:boolean)))
     (let ([name-string (symbol->string (syntax->datum #'name))])
       (with-syntax ([fn-log-critical (make-syntax/symbol stx "~A-log-critical" name-string)]
                     [fn-log-error (make-syntax/symbol stx "~A-log-error" name-string)]
                     [fn-log-warning (make-syntax/symbol stx "~A-log-warning" name-string)]
                     [fn-log-info (make-syntax/symbol stx "~A-log-info" name-string)]
                     [fn-log-debug (make-syntax/symbol stx "~A-log-debug" name-string)]
                     [fn-log-trace (make-syntax/symbol stx "~A-log-trace" name-string)])
         (define (make-log-fn name level)
           #`(define (#,name fmt . v)
               (let ([event (log-event (current-inexact-milliseconds)
                                       (quote #,level)
                                       category
                                       #f
                                       (apply format fmt v))])
                 (log-event-report event))))
         (define (make-log-identifier-fn name level)
           #`(define (#,name identifier fmt . v)
               (let ([event (log-event (current-inexact-milliseconds)
                                       (quote #,level)
                                       category
                                       identifier
                                       (apply format fmt v))])
                 (log-event-report event))))
         (let* ([with-identifier (if (attribute with-identifier)
                                     (syntax->datum (attribute with-identifier))
                                     #f)]
                [result (if with-identifier
                            #`(begin
                                #,(make-log-identifier-fn #'fn-log-critical 'critical)
                                #,(make-log-identifier-fn #'fn-log-error 'error)
                                #,(make-log-identifier-fn #'fn-log-warning 'warning)
                                #,(make-log-identifier-fn #'fn-log-info 'info)
                                #,(make-log-identifier-fn #'fn-log-debug 'debug)
                                #,(make-log-identifier-fn #'fn-log-trace 'trace))
                            #`(begin
                                #,(make-log-fn #'fn-log-critical 'critical)
                                #,(make-log-fn #'fn-log-error 'error)
                                #,(make-log-fn #'fn-log-warning 'warning)
                                #,(make-log-fn #'fn-log-info 'info)
                                #,(make-log-fn #'fn-log-debug 'debug)
                                #,(make-log-fn #'fn-log-trace 'trace)))])
           result)))]))
