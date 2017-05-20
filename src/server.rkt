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
  [server-start (-> server-config? opaque-server?)]
  [server-stop (-> opaque-server? any)]))

(provide server-config)

;; -- Types --

(struct server-config (working-dir
                       hostname
                       port
                       reusable
                       max-wait-count)
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
  ;; Stop the client threads
  (with-semaphore (opaque-server-semaphore server)
    (let ([clients (opaque-server-clients server)])
      (when (not (set-empty? clients))
        (server-log "Warning: Terminating ~A active clients." (set-count clients))
        (for ([client (in-set clients)])
          (client-stop client)))))
  ;; Log shutdown
  (server-log "Server stopped."))

;; -- Private Procedures --

(define (client-connected config semaphore clients input-port output-port)
  (with-semaphore semaphore
    (let ([client (client-start (server-config-working-dir config) input-port output-port)])
      (set-add! clients client))))

(define server-log (create-local-log "Server"))
