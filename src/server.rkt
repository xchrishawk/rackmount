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
                         [port-number port-number?]
                         [listener-reusable boolean?]
                         [listener-max-wait exact-nonnegative-integer?])]))

;; -- Structs --

(struct server-config (worker-count
                       max-thread-count
                       working-dir
                       interface
                       port-number
                       listener-reusable
                       listener-max-wait)
  #:transparent)

;; -- Public Procedures --

(define (server-run config)
  (server-log "Launching server...")
  (let (;; Launch worker places
        [workers (make-workers (server-config-worker-count config))]
        ;; Launch listener
        [listener (tcp-listen (server-config-port-number config)
                              (server-config-listener-max-wait config)
                              (server-config-listener-reusable config)
                              (server-config-interface config))])
    (server-log "Listener launched on interface ~A, port ~A."
                (or (server-config-interface config) "<any>")
                (server-config-port-number config))
    ;; Main loop
    (let loop ()
      (let ([evt (with-handlers ([exn:break? (λ (ex) 'break)])
                   (sync/enable-break listener))])
        (cond
          ;; Received a new client connection
          [(equal? evt listener)
           (let-values ([(input-port output-port) (tcp-accept listener)])
             (workers-send-task workers (list 'client input-port output-port)))
           (loop)]
          ;; Received break - shut down
          [(equal? evt 'break)
           (server-log "Received break, terminating server...")]
          ;; Unknown event?
          [else (server-log "Received unknown event (~A). Ignoring..." evt)])))
    ;; Shut down
    (tcp-close listener)
    (workers-terminate workers))
  (server-log "Server terminated."))

;; -- Private Procedures --

(define server-log
  (create-local-log "Server"))
