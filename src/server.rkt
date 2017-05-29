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
  (server-log-info (startup-message config))
  (let (;; Client ID generator
        [client-id-generator (sequence->generator (in-naturals))]
        ;; Launch worker places
        [workers (make-workers (server-config-worker-count config))]
        ;; Launch listener
        [listener (tcp-listen (server-config-port-number config)
                              (server-config-listener-max-wait config)
                              (server-config-listener-reusable config)
                              (server-config-interface config))])
    (server-log-trace "Listener launched...")
    ;; Main loop
    (let loop ()
      (let ([evt (with-handlers ([exn:break? (λ (ex) 'break)])
                   (sync/enable-break listener))])
        (cond
          ;; Received a new client connection
          [(equal? evt listener)
           (let*-values ([(identifier) (format "Client ~A" (client-id-generator))]
                         [(input-port output-port) (tcp-accept listener)]
                         [(task-spec) (client-task-spec identifier
                                                        input-port
                                                        output-port
                                                        (server-config-working-dir config)
                                                        (server-config-client-timeout config))])
             (server-log-trace "New client (~A) connected, queueing task..." identifier)
             (workers-queue-task workers task-spec))
           (loop)]
          ;; Received break - shut down
          [(equal? evt 'break)
           (server-log-debug "Received break, terminating server...")]
          ;; Unknown event?
          [else
           (server-log-error "Received unknown event (~A). Ignoring..." evt)])))
    ;; Shut down
    (tcp-close listener)
    (workers-terminate workers #:finish-tasks #f))
  (server-log-info "Server terminated."))

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
