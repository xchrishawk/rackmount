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
(require "../util/exceptions.rkt")
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
    (let* ([initial-ts (make-transaction-state
                        identifier
                        input-port
                        output-port
                        working-dir
                        (+ (current-inexact-milliseconds) 15000.0))] ; todo
           [final-ts (transaction-proc initial-ts)])
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
      ['client-disconnected
       (loop (transaction-handle-client-disconnected ts))]
      ['transaction-timed-out
       (loop (transaction-handle-transaction-timed-out ts))]
      ['transaction-cancelled
       (loop (transaction-handle-transaction-cancelled ts))]
      ['done
       (transaction-handle-done ts)])))

(define (transaction-handle-start ts)
  (update-state ts 'get-request-line))

(define (transaction-handle-get-request-line ts)
  (wait-for-line
   ts
   (λ (line)
     (displayln line)
     (update-state ts 'get-header))))

(define (transaction-handle-get-header ts)
  (wait-for-line
   ts
   (λ (line)
     (displayln line)
     (update-state ts 'get-body))))

(define (transaction-handle-get-body ts)
  (wait-for-line
   ts
   (λ (line)
     (displayln line)
     (update-state ts 'process-request))))

(define (transaction-handle-log-request ts)
  (update-state ts 'process-request))

(define (transaction-handle-process-request ts)
  (update-state ts 'send-response))

(define (transaction-handle-send-response ts)
  (update-state ts 'log-response))

(define (transaction-handle-log-response ts)
  (update-state ts 'done [result 'success]))

(define (transaction-handle-client-disconnected ts)
  (update-state ts 'done [result 'client-disconnected]))

(define (transaction-handle-transaction-timed-out ts)
  (update-state ts 'done [result 'transaction-timed-out]))

(define (transaction-handle-transaction-cancelled ts)
  (update-state ts 'done [result 'transaction-cancelled]))

(define (transaction-handle-done ts)
  (session-log-trace
   (transaction-state-identifier ts)
   "Transaction completed, result = ~A"
   (transaction-state-result ts))
  ts)

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

(define (wait-for-line ts process-line)
  (sync
   ;; Received data from client
   (handle-evt
    (read-line-evt (transaction-state-input-port ts) 'any)
    (match-lambda
      ;; Valid line
      [(? string? line) (process-line line)]
      ;; Client disconnected
      [eof (update-state ts 'client-disconnected)]))
   ;; Transaction timed out
   (handle-evt
    (alarm-evt (transaction-state-deadline ts))
    (thunk* (update-state ts 'transaction-timed-out)))
   ;; Received thread event
   (handle-evt
    (wrap-evt (thread-receive-evt) (thunk* (thread-receive)))
    (match-lambda
      ;; Transaction was cancelled, probably due to worker shutting down
      ['terminate (update-state ts 'transaction-cancelled)]
      ;; Unknown event?
      [bad-message (raise-bad-message-error bad-message)]))))

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
