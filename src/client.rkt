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
  [client-proc (-> input-port? output-port? any)]))

;; -- Structs --

(struct client-info (state
                     input-port
                     output-port)
  #:transparent)

;; -- Public Procedures --

(define (client-proc input-port output-port)
  (let ([client (client-info 'start input-port output-port)])
    (client-log "Client connected...")
    (client-loop client)
    (close-input-port input-port)
    (close-output-port output-port)
    (client-log "Client connection terminated.")))

;; -- Private Procedures --

(define (client-loop client)
  (void))

(define client-log
  (create-local-log "Client"))
