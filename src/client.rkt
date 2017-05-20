;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "log.rkt")
(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Launches a client thread.
  [client-start (-> string? input-port? output-port? (-> any) opaque-client?)]
  ;; Terminates a client thread.
  [client-stop (-> opaque-client? void?)]))

;; -- Types --

(struct opaque-client (thread))

;; -- Public Procedures --

(define (client-start working-dir input-port output-port shutdown-closure)
  (let ([client-thread
         (thread-start
          (client-log "Client connected. Client thread starting...")
          (let loop ()
            ;; Wait for the next evenet
            (let ([evt (sync (read-line-evt input-port) (thread-receive-evt))])
              (cond
                ;; Received line from client
                [(string? evt)
                 (client-log "RX: ~A" evt)
                 (loop)]
                ;; Client disconnect
                [(equal? evt eof)
                 (client-log "RX: <client disconnected>")
                 (void)]
                ;; Shutdown command
                [(and (equal? evt (thread-receive-evt))
                      (equal? (thread-receive) 'shutdown))
                   (void)]
                ;; Unknown event?
                [else (error "Unknown event??")])))
          ;; Shut down ports
          (close-input-port input-port)
          (close-output-port output-port)
          ;; Perform shutdown closure then end thread
          (shutdown-closure)
          (client-log "Client thread terminated normally."))])
    ;; Return an opaque object representing the client
    (opaque-client client-thread)))

(define (client-stop client)
  (let ([client-thread (opaque-client-thread client)])
    ;; There is a potential that the client thread may have already shut itself down
    ;; if the client disconnected, so tolerate that case.
    (when (thread-running? client-thread)
      (thread-send client-thread 'shutdown)
      (sync client-thread)
      (void))))

;; -- Private Procedures --

(define client-log (create-local-log "Client"))
