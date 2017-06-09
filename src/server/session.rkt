;;
;; session.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines procedures used to handle a session with an HTTP client. A
;; session may consist of one or more "transactions", which represent a request
;; from the client and the response sent by the server.
;;

#lang racket

;; -- Requires --

(require (for-syntax syntax/parse))
(require "../tasks/task.rkt")
(require "../util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Main client handler procedure.
  [session-proc (-> gen:task-identifier? input-port? output-port? path-string? void?)]))

;; -- Structs --

(struct transaction-state (identifier
                           input-port
                           output-port
                           working-dir
                           deadline
                           state
                           result)
  #:transparent)

;; -- Public Procedures --

(define (session-proc identifier input-port output-port working-dir)
  (session-log-trace identifier "Session started.")
  (let loop ()
    (let* ([ts (make-transaction-state
                identifier
                input-port
                output-port
                working-dir
                (+ (current-inexact-milliseconds) 15000.0))]
           [disposition (transaction-proc ts)])
      (when #f ; todo
        (loop))))
  (session-log-trace identifier "Session terminated."))

;; -- Private Procedures (Transaction State Machine) --

(define (transaction-proc ts)
  (let loop ([ts (update-state ts 'start)])
    (match (transaction-state-state ts)
      ['start
       (loop (transaction-handle-start ts))]
      ['get-request-line
       (loop (transaction-handle-get-request-line ts))]
      ['get-header
       (loop (transaction-handle-get-header ts))]
      ['get-body
       (loop (transaction-handle-get-body ts))]
      ['log-request
       (loop (transaction-handle-log-request ts))]
      ['process-request
       (loop (transaction-handle-process-request ts))]
      ['send-response
       (loop (transaction-handle-send-response ts))]
      ['log-response
       (loop (transaction-handle-log-response ts))]
      ['done
       (transaction-handle-done ts)])))

(define (transaction-handle-start ts)
  (update-state ts 'get-request-line))

(define (transaction-handle-get-request-line ts)
  (update-state ts 'get-header))

(define (transaction-handle-get-header ts)
  (update-state ts 'get-body))

(define (transaction-handle-get-body ts)
  (update-state ts 'log-request))

(define (transaction-handle-log-request ts)
  (update-state ts 'process-request))

(define (transaction-handle-process-request ts)
  (update-state ts 'send-response))

(define (transaction-handle-send-response ts)
  (update-state ts 'log-response))

(define (transaction-handle-log-response ts)
  (update-state ts 'done))

(define (transaction-handle-done ts)
  (session-log-trace
   (transaction-state-identifier ts)
   "Transaction completed, result = ~A"
   (transaction-state-result ts))
  (transaction-state-result ts))

;; -- Private Procedures (Utility) --

(define (make-transaction-state identifier
                                input-port
                                output-port
                                working-dir
                                deadline)
  (transaction-state
   identifier
   input-port
   output-port
   working-dir
   deadline
   #f
   #f))

(define-local-log session "Session" #:require-identifier #t)

;; -- Macros --

(define-syntax (update-state stx)
  (define-syntax-class field-value-pair
    #:description "field-value pair"
    (pattern (field:id value:expr)))
  (syntax-parse stx
    [(_ ts:id new-state:expr values:field-value-pair ...)
     #`(begin
         (when (not (equal? new-state (transaction-state-state ts)))
           (session-log-trace
            (transaction-state-identifier ts)
            "Transaction state is now: ~A"
            new-state))
         (struct-copy
          transaction-state
          ts
          [state new-state]
          #,@(map (λ (field value)
                    #`(#,field #,value))
                  (syntax-e #'(values.field ...))
                  (syntax-e #'(values.value ...)))))]))
