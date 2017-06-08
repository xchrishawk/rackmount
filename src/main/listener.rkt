;;
;; listener.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the listener thread, which is responsible for listening
;; for incoming TCP connections and starting new clients.
;;

#lang racket

;; -- Requires --

(require "../util/logging.rkt")
(require "../util/thread-util.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct containing arguments for the listener thread.
  [struct listener-config ([interface (or/c string? false?)]
                           [port-number port-number?]
                           [max-wait-count exact-positive-integer?]
                           [reusable boolean?]
                           [client-connected (-> input-port? output-port? any)])]

  ;; Creates a new listener object.
  [rename make-listener listener (-> listener-config? listener?)]

  ;; Terminates a listener.
  [listener-terminate (-> listener? void?)]))

;; -- Types --

(struct listener-config (interface
                         port-number
                         max-wait-count
                         reusable
                         client-connected)
  #:transparent)

(struct listener (thread))

;; -- Public Procedures --

(define (make-listener config)
  (let ([thd (thread-start (listener-proc config))])
    (listener thd)))

(define (listener-terminate listener #:synchronous [synchronous #t])
  (let ([thd (listener-thread listener)])
    (thread-send thd 'terminate)
    (when synchronous
      (sync thd)))
  (void))

;; -- Private Procedures --

;; Main thread procedure for the listener.
(define (listener-proc config)
  (listener-log-trace "Thread running.")
  ;; Open listener
  (let ([listener (tcp-listen (listener-config-port-number config)
                              (listener-config-max-wait-count config)
                              (listener-config-reusable config)
                              (listener-config-interface config))])
    ;; Log listener configuration
    (listener-log-debug (let ([text (open-output-string)])
                          (display "Listener active with configuration:" text)
                          (define (add item value)
                            (display (format "\n- ~A: ~A" item value) text))
                          (add "Interface" (or (listener-config-interface config) "(any)"))
                          (add "Port Number" (listener-config-port-number config))
                          (add "Reusable" (listener-config-reusable config))
                          (add "Max Wait Count" (listener-config-max-wait-count config))
                          (get-output-string text)))
    ;; Run the main loop
    (let loop ()
      (sync
       ;; Received thread message
       (handle-evt
        (wrap-evt (thread-receive-evt) (thunk* (thread-receive)))
        (match-lambda
          ;; Terminate command - stop looping
          ['terminate (void)]
          ;; Unrecognized message?
          [unrecognized-message
           (listener-log-error "Unrecognized thread message (~A). Ignoring..." unrecognized-message)
           (loop)]))
       ;; New client connected
       (handle-evt
        listener
        (thunk*
         (let-values ([(input-port output-port) (tcp-accept listener)])
           (listener-log-trace "New client accepted.")
           ((listener-config-client-connected config) input-port output-port)
           (loop))))))
    ;; Shut down the listener
    (tcp-close listener)
    (listener-log-debug "Listener closed."))
  (listener-log-trace "Thread terminated."))

;; Local logging function.
(define-local-log listener "Listener")