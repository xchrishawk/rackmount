;;
;; listener-thread.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the listener thread, which is responsible for listening
;; for incoming TCP connections and starting new clients.
;;

#lang racket

;; -- Requires --

(require "../util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct containing arguments for the listener thread.
  [struct listener-thread-config ([working-dir path-string?]
                                  [interface (or/c string? false?)]
                                  [port-number port-number?]
                                  [max-wait-count exact-positive-integer?]
                                  [reusable boolean?])]

  ;; Starts a new listener thread with the specified arguments.
  [listener-thread-start (-> listener-thread-config? opaque-listener-thread?)]

  ;; Synchronously stops the specified listener thread.
  [listener-thread-stop (-> opaque-listener-thread? void?)]))

;; -- Types --

(struct opaque-listener-thread (thread))

(struct listener-thread-config (working-dir
                                interface
                                port-number
                                max-wait-count
                                reusable)
  #:transparent)

;; -- Public Procedures --

(define (listener-thread-start config)
  (opaque-listener-thread (thread (λ () (listener-thread-proc config)))))

(define (listener-thread-stop listener-thread)
  (let ([thd (opaque-listener-thread-thread listener-thread)])
    (thread-send thd 'shutdown)
    (sync thd))
  (void))

;; -- Private Procedures --

;; Main thread procedure for the listener.
(define (listener-thread-proc config)
  (listener-log-trace "Listener thread started.")
  ;; Open listener
  (let ([listener (tcp-listen (listener-thread-config-port-number config)
                              (listener-thread-config-max-wait-count config)
                              (listener-thread-config-reusable config)
                              (listener-thread-config-interface config))])
    ;; Log listener configuration
    (listener-log-debug (let ([text (open-output-string)])
                          (display "Listener active with configuration:" text)
                          (define (add item value)
                            (display (format "\n- ~A: ~A" item value) text))
                          (add "Interface" (or (listener-thread-config-interface config) "(any)"))
                          (add "Port Number" (listener-thread-config-port-number config))
                          (add "Reusable" (listener-thread-config-reusable config))
                          (add "Max Wait Count" (listener-thread-config-max-wait-count config))
                          (get-output-string text)))
    ;; Enter the listener's main loop
    (let loop ()
      ;; Wait for either a thread message or a new client connection
      (match (sync (wrap-evt (thread-receive-evt) (λ (evt) (thread-receive)))
                   listener)
        ;; Received shutdown command - exit the loop
        ['shutdown (void)]
        ;; Client connected!
        [tcp-listener?
         (listener-log-trace "New client accepted.")
         (let-values ([(input-port output-port) (tcp-accept listener)])
           (displayln "Hello!" output-port)
           (close-input-port input-port)
           (close-output-port output-port))
         (loop)]))
    ;; Shut down the listener
    (tcp-close listener)
    (listener-log-debug "Listener closed."))
  (listener-log-trace "Listener thread terminating."))

;; Local logging procedure.
(define-local-log listener "Main Listener")
