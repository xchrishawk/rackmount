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
(require "../http/http-request.rkt")
(require "../http/http-response.rkt")
(require "../http/http-response-lib.rkt")
(require "../tasks/task.rkt")
(require "../util/exceptions.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Predicate returning #t if the argument is a valid session timeout.
  [session-timeout? (-> any/c boolean?)]

  ;; Main client handler procedure.
  [session-proc (-> gen:task-identifier?
                    input-port?
                    output-port?
                    path-string?
                    session-timeout?
                    void?)]))

;; -- Structs --

(struct transaction-state (identifier
                           input-port
                           output-port
                           working-dir
                           deadline
                           state
                           request
                           response
                           result)
  #:transparent)

;; -- Public Procedures --

(define session-timeout?
  (or/c false? (and/c real? positive?)))

(define (session-proc identifier input-port output-port working-dir timeout)
  (session-log-trace identifier "Session started.")
  (let ([deadline
         (ifmap ([timeout timeout])
           (+ (current-inexact-milliseconds) timeout))])
    (let loop ()
      ;; Run the state machine and get the final result
      (let* ([initial-ts (make-transaction-state
                          identifier
                          input-port
                          output-port
                          working-dir
                          deadline)]
             [final-ts (transaction-proc initial-ts)])
        ;; Keep looping if appropriate
        (when (should-continue-session-with-transaction-result
               (transaction-state-result final-ts))
          (loop)))))
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
      ['valid-request
       (loop (transaction-handle-valid-request ts))]
      ['invalid-request
       (loop (transaction-handle-invalid-request ts))]
      ['send-response
       (loop (transaction-handle-send-response ts))]
      ['client-disconnected
       (loop (transaction-handle-client-disconnected ts))]
      ['session-timed-out
       (loop (transaction-handle-session-timed-out ts))]
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
     (let-values ([(valid updated-request)
                   (http-request-parse-request-line
                    (transaction-state-request ts)
                    line)])
       (update-state
        ts
        (if valid 'get-header 'invalid-request)
        [request updated-request])))))

(define (transaction-handle-get-header ts)
  (wait-for-line
   ts
   (λ (line)
     (if (http-request-is-end-header-line? line)
         (update-state ts 'get-body)
         (let-values ([(valid updated-request)
                       (http-request-parse-header-line
                        (transaction-state-request ts)
                        line)])
           (update-state
            ts
            (if valid 'get-header 'invalid-request)
            [request updated-request]))))))

(define (transaction-handle-get-body ts)
  ;; TODO
  (update-state ts 'valid-request))

(define (transaction-handle-valid-request ts)
  (log-valid-request ts)
  (update-state
   ts
   'send-response
   [response (http-response-ok
              #:entity #"hi there"
              #:extra-headers (hash "Content-Length" "8"))]
   [result 'success]))

(define (transaction-handle-invalid-request ts)
  (log-invalid-request ts)
  (update-state
   ts
   'send-response
   [response (http-response-bad-request)]
   [result 'invalid-request]))

(define (transaction-handle-send-response ts)
  (let ([response-bytes (http-response->bytes (transaction-state-response ts))])
    (write-bytes response-bytes (transaction-state-output-port ts))
    (flush-output (transaction-state-output-port ts)))
  (log-response ts)
  (update-state ts 'done))

(define (transaction-handle-client-disconnected ts)
  (update-state ts 'done [result 'client-disconnected]))

(define (transaction-handle-session-timed-out ts)
  (update-state ts 'done [result 'session-timed-out]))

(define (transaction-handle-transaction-cancelled ts)
  (update-state ts 'done [result 'transaction-cancelled]))

(define (transaction-handle-done ts)
  (session-log-trace
   (transaction-state-identifier ts)
   "Transaction completed, result = ~A"
   (transaction-state-result ts))
  ts)

;; -- Private Procedures (Utility) --

;; Creates a new, default transaction-state struct.
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
   (make-http-request)
   #f
   #f))

;; Logs a valid request.
(define (log-valid-request ts)
  (let ([message (open-output-string)]
        [request (transaction-state-request ts)])
    (define (add item value #:indent [indent 0])
      (display (format "\n~A- ~A: ~A" (make-string indent #\space) item value) message))
    (display "Received valid request as follows..." message)
    (add "Method" (http-request-method request))
    (add "URI" (http-request-uri request))
    (add "HTTP Version"
         (format
          "~A.~A"
          (http-request-major-version request)
          (http-request-minor-version request)))
    (add "Headers" (string-empty))
    (for ([(name value) (in-hash (http-request-headers request))])
      (add name value #:indent 2))
    (session-log-info
     (transaction-state-identifier ts)
     (get-output-string message))))

;; Logs an invalid request.
(define (log-invalid-request ts)
  (let ([message (open-output-string)]
        [request (transaction-state-request ts)])
    (displayln "Received invalid request as follows..." message)
    (display (http-request-raw request) message)
    (session-log-info
     (transaction-state-identifier ts)
     (get-output-string message))))

;; Logs a sent response.
(define (log-response ts)
  (let ([message (open-output-string)]
        [response (transaction-state-response ts)])
    (define (add item value #:indent [indent 0])
      (display (format "\n~A- ~A: ~A" (make-string indent #\space) item value) message))
    (display "Sent response as follows..." message)
    (add "Status Code" (http-response-status-code response))
    (add "Reason" (http-response-reason response))
    (add "HTTP Version"
         (format
          "~A.~A"
          (http-response-major-version response)
          (http-response-minor-version response)))
    (add "Headers" (string-empty))
    (for ([(name value) (in-hash (http-response-headers response))])
      (add name value #:indent 2))
    (session-log-info
     (transaction-state-identifier ts)
     (get-output-string message))))

;; Returns #t if the session should continue with the specified result for the
;; previous transaction.
(define (should-continue-session-with-transaction-result result)
  (match result
    [(or 'success) #t]
    [else #f]))

;; Waits for a line from the client. If a line is received, calls process-line with
;; the line as the argument, and returns the result. Otherwise, returns an updated
;; transaction-state struct for the transaction disposition.
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
   ;; Session timed out
   (handle-evt
    (let ([deadline (transaction-state-deadline ts)])
      (if deadline (alarm-evt deadline) never-evt))
    (thunk* (update-state ts 'session-timed-out)))
   ;; Received thread event
   (handle-evt
    (wrap-evt (thread-receive-evt) (thunk* (thread-receive)))
    (match-lambda
      ;; Transaction was cancelled, probably due to worker shutting down
      ['terminate (update-state ts 'transaction-cancelled)]
      ;; Unknown event?
      [bad-message (raise-bad-message-error bad-message)]))))

;; Local logging procedures.
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
