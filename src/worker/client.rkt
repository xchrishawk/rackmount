;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the state machine used to interact with clients through a
;; single connection instance.
;;

#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax racket/syntax))
(require "../http/http-request.rkt")
(require "../http/http-response.rkt")
(require "../http/hypertext.rkt")
(require "../util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Predicate returning #t if the argument is a valid client timeout.
  [client-timeout? (-> any/c boolean?)]

  ;; Runs the main client handling procedure.
  [client-proc (-> string?
                   input-port?
                   output-port?
                   path-string?
                   client-timeout?
                   any)]))

;; -- Structs --

(struct client-info (state
                     identifier
                     input-port
                     output-port
                     working-dir
                     timeout
                     startup-time
                     request-method
                     request-uri
                     request-version-major
                     request-version-minor
                     request-headers
                     request-body
                     response
                     disposition)
  #:transparent)

;; -- Public Procedures --

(define client-timeout?
  (or/c (and/c real? number?) false?))

(define (client-proc identifier input-port output-port working-dir timeout)
  (let ([client (make-client-info identifier input-port output-port working-dir timeout)])
    (client-loop client)))

;; -- Private Procedures (State Machine) --

(define (client-loop client)
  (match (client-info-state client)
    ;; Initial startup state.
    ['start (client-loop (handle-start client))]
    ;; Read request line.
    ['get-request-line (client-loop (handle-get-request-line client))]
    ;; Read header line.
    ['get-header (client-loop (handle-get-header client))]
    ;; Read request body.
    ['get-body (client-loop (handle-get-body client))]
    ;; Log request.
    ['log-request (client-loop (handle-log-request client))]
    ;; Process request.
    ['process-request (client-loop (handle-process-request client))]
    ;; Send response.
    ['send-response (client-loop (handle-send-response client))]
    ;; Log response.
    ['log-response (client-loop (handle-log-response client))]
    ;; Disconnect from client.
    ['transaction-complete (client-loop (handle-transaction-complete client))]
    ;; State machine is complete - return.
    ['done (void)]
    ;; Unknown state - should not happen.
    [else (error "Invalid state!" (client-info-state client))]))

(define (handle-start client)
  (client-log-info (client-info-identifier client) "Client connected.")
  (update-client client 'get-request-line))

(define (handle-get-request-line client)
  (get-line client (line)
            (match (parse-request-line line)
              ;; Request line is valid
              [(list (? string? method)
                     (? string? uri)
                     (? exact-nonnegative-integer? version-major)
                     (? exact-nonnegative-integer? version-minor))
               (update-client client
                              'get-header
                              [request-method method]
                              [request-uri uri]
                              [request-version-major version-major]
                              [request-version-minor version-minor])]
              ;; Request line is invalid
              [else
               (update-client client
                              'send-response
                              [response (http-response-bad-request)])])))

(define (handle-get-header client)
  (get-line client (line)
            (match (parse-header-line line)
              ;; Header line is valid and contains data
              [(list (? string? header-name)
                     (? string? header-value))
               (update-client client
                              'get-header
                              [request-headers
                               (let ([prev-headers (client-info-request-headers client)])
                                 (if prev-headers
                                     (hash-set prev-headers header-name header-value)
                                     (hash header-name header-value)))])]
              ;; Header line is valid and is empty, signifying end of headers
              ['no-more-headers
               (update-client client 'get-body)]
              ;; Header line is invalid
              [else
               (update-client client 'send-response [response (http-response-bad-request)])])))

(define (handle-get-body client)
  ;; TODO
  (update-client client 'log-request))

(define (handle-log-request client)
  (let ([event (open-output-string)])
    (define (add key [value ""] #:indent [indent 0])
      (display (format "\n~A- ~A: ~A" (make-string indent #\space) key value) event))
    (display "Received HTTP request with following parameters:" event)
    (add "Method" (client-info-request-method client))
    (add "URI" (client-info-request-uri client))
    (add "HTTP Major Version" (client-info-request-version-major client))
    (add "HTTP Minor Version" (client-info-request-version-minor client))
    (add "Headers")
    (for ([(key value) (in-hash (client-info-request-headers client))])
      (add key value #:indent 2))
    (client-log-info
     (client-info-identifier client)
     (get-output-string event)))
  (update-client client 'process-request))

(define (handle-process-request client)
  ;; TODO - send dummy response for now
  (update-client client
                 'send-response
                 [response (http-response-ok/utf-8
                            (hypertext
                             (html "Hello!")))]))

(define (handle-send-response client)
  (let ([output-port (client-info-output-port client)]
        [response-bytes (http-response->bytes (client-info-response client))])
    (write-bytes response-bytes output-port)
    (flush-output output-port))
  (update-client client 'log-response [disposition 'response-sent]))

(define (handle-log-response client)
  ;; TODO
  (update-client client 'transaction-complete))

(define (handle-transaction-complete client)
  ;; TODO
  (client-log-info
   (client-info-identifier client)
   "Transaction complete, disposition: ~A"
   (client-info-disposition client))
  (update-client client 'done))

;; -- Private Procedures (Client Struct Management) --

;; Create a new client-info struct with the specified values.
(define (make-client-info identifier
                          input-port
                          output-port
                          working-dir
                          timeout)
  (client-info 'start
               identifier
               input-port
               output-port
               working-dir
               timeout
               (current-inexact-milliseconds)
               #f #f #f #f #f #f #f #f))

;; Create a new client-info struct with basic data copied from the specified struct.
(define (reset-client-info client)
  (make-client-info (client-info-identifier client)
                    (client-info-input-port client)
                    (client-info-output-port client)
                    (client-info-working-dir client)
                    (client-info-timeout client)))

;; -- Private Procedures (Logging) --

;; Local logging procedure.
(define-local-log client "Client" #:with-identifier #t)

;; -- Private Procedures (Macro Helpers) --

;;
;; Read a single line from the client. Returns:
;;
;; - A string if a line was successfully received
;; - client-disconnected if the client disconnected from the remote end
;; - worker-terminated if the worker is terminating
;; - timed-out if the client times out
;;
(define (get-line-helper client)
  (let* ([line-evt (read-line-evt (client-info-input-port client) 'any)]
         [message-evt (thread-receive-evt)]
         [evt (sync/timeout (client-timeout-remaining client)
                            line-evt
                            message-evt)])
    (match evt
      ;; Client sent a line of text.
      [(? string? line) line]
      ;; Client disconnected from remote end.
      [(? (λ (evt) (equal? evt eof)) _) 'client-disconnected]
      ;; Received shutdown command
      [(? (λ (evt) (equal? evt message-evt)) _)
       (match (thread-receive)
         ['shutdown 'worker-terminated])]
      ;; Connection timed out
      [#f 'timed-out])))

;; Returns the timeout remaining for the specified client, or #f if there is no timeout.
(define (client-timeout-remaining client)
  (let ([startup-time (client-info-startup-time client)]
        [timeout (client-info-timeout client)])
    (if timeout
        (/ (- (+ startup-time timeout)
              (current-inexact-milliseconds))
           1000.0)
        #f)))

;; -- Macros --

;; Updates a client's state, and optionally any number of fields.
(define-syntax (update-client stx)
  (syntax-case stx ()
    [(_ client new-state (field value) ...)
     #`(begin
         (when (not (equal? new-state (client-info-state client)))
           (client-log-trace
            (client-info-identifier client)
            "State is now: ~A"
            new-state))
         (struct-copy
          client-info
          client
          [state new-state]
          #,@(map (λ (field value)
                    #`(#,field #,value))
                  (syntax-e #'(field ...))
                  (syntax-e #'(value ...)))))]))

;; Gets a line from the client. If a line cannot be received, transition the
;; client to a failed state.
(define-syntax (get-line stx)
  (syntax-case stx ()
    [(_ client (var) body0 body ...)
     #`(match (get-line-helper client)
         [(? string? var)
          body0
          body ...]
         [(? symbol? disp)
          (update-client client 'transaction-complete [disposition disp])])]))
