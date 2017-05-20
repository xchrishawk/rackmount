;;
;; listener.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "log.rkt")
(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Starts a listener.
  [listener-start (-> (or/c string? false?)
                      port-number?
                      boolean?
                      exact-nonnegative-integer?
                      (-> input-port? output-port? any)
                      opaque-listener?)]
  ;; Stops a listener.
  [listener-stop (-> opaque-listener? void?)]))

;; -- Types --

(struct opaque-listener (thread))

;; -- Public Procedures --

(define (listener-start hostname
                        port
                        reusable
                        max-wait
                        client-connected)
  (let ([listener-thread
         (thread-start
          ;; Start the listener
          (let ([listener (tcp-listen port max-wait reusable hostname)])
            ;; Log startup
            (listener-log "Listener created on ~A port ~A. Listener thread starting..."
                          (or hostname "any interface")
                          port)
            ;; Main loop
            (let loop ()
              ;; Wait for the next event
              (let ([evt (sync listener (thread-receive-evt))])
                (cond
                  ;; Client connected
                  [(equal? evt listener)
                   (let-values ([(input-port output-port) (tcp-accept listener)])
                     (client-connected input-port output-port))
                   (loop)]
                  ;; Shutdown request
                  [(and (equal? evt (thread-receive-evt))
                        (equal? (thread-receive) 'shutdown))
                   (void)]
                  ;; Unknown event??
                  [else (error "Unknown event!")])))
            ;; Shutdown and log
            (tcp-close listener)
            (listener-log "Listener thread terminated normally.")))])
    (opaque-listener listener-thread)))

(define (listener-stop listener)
  (let ([listener-thread (opaque-listener-thread listener)])
    (thread-send listener-thread 'shutdown)
    (sync listener-thread)
    (void)))

;; -- Private Procedures --

;; Logging procedure for listener threads.
(define listener-log (create-local-log "Listener"))
