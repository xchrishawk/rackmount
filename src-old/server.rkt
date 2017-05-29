;;
;; server.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the main server procedure, which runs on the main startup
;; thread for the server application. This acts as the "listener" thread which
;; accepts new clients and dispatches them to workers for servicing.
;;

#lang racket

;; -- Requires --

(require racket/generator)
(require "client-task.rkt")
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
                         [working-dir path-string?]
                         [interface (or/c string? false?)]
                         [port-number port-number?]
                         [client-timeout (or/c (and/c number? positive?) false?)]
                         [listener-reusable boolean?]
                         [listener-max-wait exact-nonnegative-integer?])]))

;; -- Structs --

(struct server-config (worker-count
                       working-dir
                       interface
                       port-number
                       client-timeout
                       listener-reusable
                       listener-max-wait)
  #:prefab)

;; -- Public Procedures --

(define (server-run config)
  ;; TODO
  (void))

;; -- Private Procedures --

(define (startup-message config)
  (let ([output (open-output-string)])
    (parameterize ([current-output-port output])
      (display "Launching server with configuration:")
      (define (add key value [default #f])
        (display (format "\n- ~A: ~A" key (or value default))))
      (add "Worker Count" (server-config-worker-count config))
      (add "Working Directory" (server-config-working-dir config))
      (add "Interface" (server-config-interface config) "(any)")
      (add "Port Number" (server-config-port-number config))
      (add "Client Timeout" (server-config-client-timeout config) "(none)")
      (add "Listener Reusable" (server-config-listener-reusable config))
      (add "Listener Max Waiting" (server-config-listener-max-wait config)))
    (get-output-string output)))

(define-local-log server "Server")
