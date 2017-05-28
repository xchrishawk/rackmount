;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the state machine used to interact with clients through a
;; single connection instance.
;;

#lang racket

;; -- Requires --

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
                     response)
  #:transparent)

;; -- Public Procedures --

(define (client-proc identifier input-port output-port)
  (let ([client (make-client-info identifier input-port output-port)])
    (client-log client "Client connected...")
    (client-loop client)
    (close-input-port input-port)
    (close-output-port output-port)
    (client-log client "Client connection terminated.")))

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
    ;; Process request.
    ['process-request (client-loop (handle-process-request client))]
    ;; Send response
    ['send-response (client-loop (handle-send-response client))]
    ;; State machine is complete - return.
    ['done (void)]
    ;; Unknown state - should not happen.
    [else (error "Invalid state!" (client-info-state client))]))

(define (handle-start client)
  (update-client client 'get-request-line))

(define (handle-get-request-line client)
  (let ([line (read-line-from-client client)])
    (if line
        ;; Got a line from the client
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
                          [response (http-response-bad-request)])])
        ;; No line - drop connection
        (update-client client 'done))))

(define (handle-get-header client)
  (let ([line (read-line-from-client client)])
    (if line
        ;; Got a line from the client
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
           (update-client client 'send-response [response (http-response-bad-request)])])
        ;; No line - drop connection
        (update-client client 'done))))

(define (handle-get-body client)
  ;; TODO - skip for now
  (update-client client 'process-request))

(define (handle-process-request client)
  ;; TODO - dummy response
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
  (update-client client 'done))

;; -- Private Procedures (Helpers) --

;; Create a new client-info struct with the specified values.
(define (make-client-info identifier input-port output-port)
  (client-info 'start identifier input-port output-port #f #f #f #f #f #f #f))

;; Create a new client-info struct with basic data copied from the specified struct.
(define (reset-client-info client)
  (make-client-info (client-info-identifier client)
                    (client-info-input-port client)
                    (client-info-output-port client)))

;; Read a single line from the client. Returns #f if no line could be read.
(define (read-line-from-client client)
  (let ([evt (sync (read-line-evt (client-info-input-port client) 'any)
                   (thread-receive-evt))])
    (match evt
      ;; Client sent a line of text.
      [(? string? line) line]
      ;; Client disconnected from remote end.
      [(? (λ (evt) (equal? evt eof)) _) #f]
      ;; Received shutdown command
      [(? (λ (evt) (equal? evt (thread-receive-evt))) _)
       (match (thread-receive)
         ['shutdown #f])])))

;; Local logging procedure.
(define (client-log client fmt . v)
  (apply rackmount-log "Client" (client-info-identifier client) fmt v))

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
