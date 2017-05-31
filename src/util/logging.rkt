;;
;; logging.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines shared types and procedures for logging. Log events are
;; stored in a synchronized queue, represented as a Racket async buffered channel.
;; A consumer thread is reponsible for retrieving and processing the events
;; stored in the queue:
;;
;; - In worker places, events are forwarded to the main place through a channel
;;   so they can be added to the main place's log queue.
;; - In the main place, events are reported, either to console output, a backing
;;   store, or other endpoint as configured by the user.
;;

#lang racket

;; -- Requires --

(require racket/async-channel)
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
(require (for-syntax "syntax.rkt"))

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

  ;; Enqueues an event into the logging queue.
  [log-event-enqueue (-> log-event? void?)]

  ;; Immediately returns either the next log event available in the queue, or
  ;; #f if there are no events available. Does not block.
  [log-event-dequeue (-> (or/c log-event? false?))]

  ;; Returns a synchronizable event which is ready for synchronization any time
  ;; there is a log event available in the logging queue. The synchronization
  ;; result of the event is the dequeued event.
  [log-event-dequeue-evt (-> evt?)]

  ;; Returns a formatted string for the specified log event.
  [log-event->string (-> log-event? string?)]

  ;; Predicate returning #t if the argument is a valid log level.
  [log-event-level? (-> any/c boolean?)]

  ;; Returns #t if the specified log event level is enabled, based on the
  ;; specified minimum log event level.
  [log-event-level-enabled? (-> log-event-level? log-event-level? boolean?)]

  ;; Predicate returning #t if the argument is a list representation of a
  ;; log-event struct.
  [log-event-list? (-> any/c boolean?)]

  ;; Converts a log-event to a list representation.
  [log-event->list (-> log-event? log-event-list?)]

  ;; Converts a list representation to a log event.
  [list->log-event (-> log-event-list? log-event?)]))

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

;; Main logging queue.
(define log-event-queue (make-async-channel))

;; -- Public Procedures --

(define (log-event-enqueue log-event)
  (async-channel-put log-event-queue log-event))

(define (log-event-dequeue)
  (async-channel-try-get log-event-queue))

(define (log-event-dequeue-evt)
  ;; we use wrap-event to avoid leaking a reference to the queue
  (wrap-evt log-event-queue identity))

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

(define log-event-list?
  (list/c 'log-event inexact? log-event-level? string? (or/c string? false?) string?))

(define (log-event->list log-event)
  (list 'log-event
        (log-event-date log-event)
        (log-event-level log-event)
        (log-event-category log-event)
        (log-event-identifier log-event)
        (log-event-text log-event)))

(define (list->log-event lst)
  (apply log-event (rest lst)))

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
                     [fn-log-trace (make-syntax/symbol stx "~A-log-trace" name-string)]
                     [identifier-arg (generate-temporary "identifier")]
                     [format-arg (generate-temporary "format")]
                     [v-arg (generate-temporary "v")])
         (define (make-log-fn name level)
           `(define (,name ,#'format-arg . ,#'v-arg)
              (log-event-enqueue (log-event (current-inexact-milliseconds)
                                            (quote ,level)
                                            ,#'category
                                            #f
                                            (apply format ,#'format-arg ,#'v-arg)))))
         (define (make-log-identifier-fn name level)
           `(define (,name ,#'identifier-arg ,#'format-arg . ,#'v-arg)
              (log-event-enqueue (log-event (current-inexact-milliseconds)
                                            (quote ,level)
                                            ,#'category
                                            ,#'identifier-arg
                                            (apply format ,#'format-arg ,#'v-arg)))))
         (let* ([with-identifier (if (attribute with-identifier)
                                     (syntax->datum (attribute with-identifier))
                                     #f)]
                [result (if with-identifier
                            `(begin
                               ,(make-log-identifier-fn #'fn-log-critical 'critical)
                               ,(make-log-identifier-fn #'fn-log-error 'error)
                               ,(make-log-identifier-fn #'fn-log-warning 'warning)
                               ,(make-log-identifier-fn #'fn-log-info 'info)
                               ,(make-log-identifier-fn #'fn-log-debug 'debug)
                               ,(make-log-identifier-fn #'fn-log-trace 'trace))
                            `(begin
                               ,(make-log-fn #'fn-log-critical 'critical)
                               ,(make-log-fn #'fn-log-error 'error)
                               ,(make-log-fn #'fn-log-warning 'warning)
                               ,(make-log-fn #'fn-log-info 'info)
                               ,(make-log-fn #'fn-log-debug 'debug)
                               ,(make-log-fn #'fn-log-trace 'trace)))])
           (datum->syntax stx result))))]))

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)

  ;; -- Test Cases --

  ;; Test that the logging queue operates as expected.
  (test-case "log-event-queue"
    (let ([semaphore (make-semaphore)]
          [rx-text #f])
      (thread (λ ()
                (let ([log-event (sync (log-event-dequeue-evt))])
                  (set! rx-text (log-event-text log-event))
                  (semaphore-post semaphore))))
      (check-equal? rx-text #f)
      (log-event-enqueue (log-event (current-seconds) 'critical "test" #f "test"))
      (semaphore-wait semaphore)
      (check-equal? rx-text "test")))

  ;; Test for the log-event-level? procedure.
  (test-case "log-event-level?"
    (for ([level (in-list (list 'critical 'error 'warning 'info 'debug 'trace))])
      (check-true (log-event-level? level)))
    (check-false (log-event-level? 'bad)))

  ;; Test that the minimum-log-event-level parameter and log-event-level-enabled?
  ;; operate as expected.
  (test-case "log-event-level-enabled?"
    (check-true (log-event-level-enabled? 'critical 'error))
    (check-true (log-event-level-enabled? 'error 'error))
    (check-false (log-event-level-enabled? 'warning 'error))
    (check-true (log-event-level-enabled? 'info 'debug))
    (check-true (log-event-level-enabled? 'debug 'debug))
    (check-false (log-event-level-enabled? 'trace 'debug)))

  ;; Test for the log-event-level->integer procedure.
  (test-case "log-event-level->integer"
    (define (test level value)
      (check-equal? (log-event-level->integer level) value))
    (test 'critical 0)
    (test 'error 1)
    (test 'warning 2)
    (test 'info 3)
    (test 'debug 4)
    (test 'trace 5))

  ;; Test case for the log-event-level->string procedure.
  (test-case "log-event-level->string"
    (define (test level value)
      (check-equal? (log-event-level->string level) value))
    (test 'critical "Critical")
    (test 'error "Error")
    (test 'warning "Warning")
    (test 'info "Info")
    (test 'debug "Debug")
    (test 'trace "Trace")))
