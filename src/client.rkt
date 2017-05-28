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
(require "http-request.rkt")
(require "http-response.rkt")
(require "hypertext.rkt")
(require "log.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Runs the main client handling procedure.
  [client-proc (-> string? input-port? output-port? any)]))

;; -- Structs --

(struct client-info (state
                     identifier
                     input-port
                     output-port
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

(define (client-proc identifier input-port output-port)
  (let ([client (make-client-info identifier input-port output-port)])
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
    ['disconnect (client-loop (handle-disconnect client))]
    ;; State machine is complete - return.
    ['done (void)]
    ;; Unknown state - should not happen.
    [else (error "Invalid state!" (client-info-state client))]))

(define (handle-start client)
  (client-log client "Client connected...")
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
  ;; TODO
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
  (update-client client 'disconnect))

(define (handle-disconnect client)
  (let ([disposition (client-info-disposition client)])
    (when (not disposition)
      (error "Disposition must be set prior to disconnecting!"))
    (close-input-port (client-info-input-port client))
    (close-output-port (client-info-output-port client))
    (client-log client "Connection terminated, disposition: ~A" disposition))
  (update-client client 'done))

;; -- Private Procedures (Client Struct Management) --

;; Create a new client-info struct with the specified values.
(define (make-client-info identifier input-port output-port)
  (client-info 'start identifier input-port output-port #f #f #f #f #f #f #f #f))

;; Create a new client-info struct with basic data copied from the specified struct.
(define (reset-client-info client)
  (make-client-info (client-info-identifier client)
                    (client-info-input-port client)
                    (client-info-output-port client)))

;; -- Private Procedures (Logging) --

;; Logs a request from the client.
(define (client-log-request client)
  (void))

;; Logs a response to the client.
(define (client-log-response client)
  (void))

;; Local logging procedure.
(define (client-log client fmt . v)
  (apply rackmount-log "Client" (client-info-identifier client) fmt v))

;; -- Private Procedures (Macro Helpers) --

;; Read a single line from the client. Returns:
;; - A string if a line was successfully received
;; - client-disconnected if the client disconnected from the remote end
;; - worker-terminated if the worker is terminating
(define (get-line-helper client)
  (let ([evt (sync (read-line-evt (client-info-input-port client) 'any)
                   (thread-receive-evt))])
    (match evt
      ;; Client sent a line of text.
      [(? string? line) line]
      ;; Client disconnected from remote end.
      [(? (λ (evt) (equal? evt eof)) _) 'client-disconnected]
      ;; Received shutdown command
      [(? (λ (evt) (equal? evt (thread-receive-evt))) _)
       (match (thread-receive)
         ['shutdown 'worker-terminated])])))

;; -- Macros --

;; Updates a client's state, and optionally any number of fields.
(define-syntax (update-client stx)
  (syntax-case stx ()
    [(_ client state (field value) ...)
     (let* ([field-clauses (map list
                                (syntax-e #'(field ...))
                                (syntax-e #'(value ...)))]
            [result `(begin
                       (when (not (equal? ,#'state (client-info-state ,#'client)))
                         (client-log ,#'client "State is now: ~A" ,#'state))
                       (struct-copy client-info
                                    ,#'client
                                    (state ,#'state)
                                    ,@field-clauses))])
       (datum->syntax stx result))]))

;; Gets a line from the client. If a line cannot be received, transition the
;; client to a failed state.
(define-syntax (get-line stx)
  (syntax-case stx ()
    [(_ client (var) body0 body ...)
     (with-syntax ([disp-var (generate-temporary)])
       (let ([result `(match (get-line-helper ,#'client)
                        [(? string? ,#'var)
                         ,#'body0
                         ,@#'(body ...)]
                        [(? (λ (x) (or (equal? x 'client-disconnected)
                                       (equal? x 'worker-terminated)))
                            ,#'disp-var)
                         (update-client ,#'client 'disconnect [disposition ,#'disp-var])])])
         (datum->syntax stx result)))]))
