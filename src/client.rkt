;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the state machine used to interact with clients through a
;; single connection instance.
;;

#lang racket

;; -- Requires --

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
                     output-port)
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
  (update-client-state client 'get-request-line))

(define (handle-get-request-line client)
  (let ([line (read-line-from-client client)])
    (if line
        (begin
          (displayln line)
          (update-client-state client 'get-header))
        (update-client-state client 'done))))

(define (handle-get-header client)
  (update-client-state client 'get-body))

(define (handle-get-body client)
  (update-client-state client 'process-request))

(define (handle-process-request client)
  (update-client-state client 'send-response))

(define (handle-send-response client)
  (update-client-state client 'done))

;; -- Private Procedures (Helpers) --

;; Create a new client-info struct with the specified values.
(define (make-client-info identifier input-port output-port)
  (client-info 'start
               identifier
               input-port
               output-port))

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

;; Returns a copy of the specified client-info struct with the state updated.
(define (update-client-state client state)
  (client-log client "State is now: ~A" state)
  (struct-copy client-info client [state state]))

;; Local logging procedure.
(define (client-log client fmt . v)
  (apply rackmount-log "Client" (client-info-identifier client) fmt v))
