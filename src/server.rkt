;;
;; server.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "client.rkt")
(require "listener.rkt")
(require "log.rkt")
(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Starts a server instance.
  [server-start (-> server-config? opaque-server?)]
  ;; Stops a server instance.
  [server-stop (-> opaque-server? any)]
  ;; Returns a syncable event firing when the listener thread terminates for any reason.
  [server-listener-terminated-evt (-> opaque-server? evt?)]))

(provide
 (contract-out
  ;; Struct representing the configuration used to start a server.
  [struct server-config ([working-dir string?]
                         [hostname (or/c string? false?)]
                         [port port-number?]
                         [reusable boolean?]
                         [max-wait-count exact-nonnegative-integer?])]))

;; -- Types --

(struct server-config (working-dir		; Working directory for the server
                       hostname			; Host name for the interface to bind to, or #f for any
                       port			; Port number to bind to
                       reusable			; Is the listener port reusable?
                       max-wait-count)		; Max number of waiting clients
  #:transparent)

(struct opaque-server (semaphore clients listener))

;; -- Public Procedures --

(define (server-start config)
  (server-log "Server starting...")
  (let* ([semaphore (make-semaphore 1)]
         [clients (mutable-set)]
         [inner-client-connected (λ (input-port output-port)
                                   (client-connected config semaphore clients input-port output-port))]
         [listener (listener-start (server-config-hostname config)
                                   (server-config-port config)
                                   (server-config-reusable config)
                                   (server-config-max-wait-count config)
                                   inner-client-connected)])
    (opaque-server semaphore clients listener)))

(define (server-stop server)
  ;; Stop the listener thread
  (listener-stop (opaque-server-listener server))
  ;; Create a local copy of the clients set in case it gets mutated while we're iterating through it
  (let ([clients (with-semaphore (opaque-server-semaphore server)
                   (set-copy (opaque-server-clients server)))])
    ;; Terminate any clients which are left
    (when (not (set-empty? clients))
      (server-log "Warning: Terminating ~A active clients." (set-count clients))
      (for ([client (in-set clients)])
        (client-stop client))))
  ;; Log shutdown
  (server-log "Server terminated normally."))

(define (server-listener-terminated-evt server)
  (listener-terminated-evt (opaque-server-listener server)))

;; -- Private Procedures --

;; Handles client connection.
(define (client-connected config semaphore clients input-port output-port)
  ;; We have to use a delayed value for the "client" token in the shutdown closure
  (let-values ([(get-client set-client) (delayed)])
    (let ([client (client-start (server-config-working-dir config)
                                input-port
                                output-port
                                (λ ()
                                  (with-semaphore semaphore
                                    (set-remove! clients (get-client)))))])
      ;; Add the client to the client set
      (with-semaphore semaphore
        (set-add! clients client))
      ;; Set the delayed value so the shutdown closure works
      (set-client client))))

;; Logs an event to the "Server" category.
(define server-log
  (create-local-log "Server"))
