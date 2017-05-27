;;
;; server.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "log.rkt")
(require "utility.rkt")
(require "worker.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Main server procedure.
  [server-run (-> server-config? any)]
  ;; Struct containing server configuration.
  [struct server-config ([worker-count exact-positive-integer?]
                         [max-thread-count exact-positive-integer?]
                         [working-dir string?]
                         [interface (or/c string? false?)]
                         [port-number port-number?])]))

;; -- Structs --

(struct server-config (worker-count
                       max-thread-count
                       working-dir
                       interface
                       port-number)
  #:transparent)

;; -- Public Procedures --

(define (server-run config)
  (server-log "Launching server...")
  (let ([workers (make-workers (server-config-worker-count config))])
    (for ([x (in-range 8)])
      (workers-send-job workers 'heavy-crunch))
    (workers-terminate workers))
  (server-log "Server terminated."))

;; -- Private Procedures --

(define server-log
  (create-local-log "Server"))
