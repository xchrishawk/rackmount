;;
;; listener.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the listener thread, which is responsible for listening
;; for incoming TCP connections and starting new clients.
;;

#lang racket

;; -- Requires --

(require "../main/define-thread.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")
(require "../util/sync-loop.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct containing arguments for the listener thread.
  [struct listener-config ([interface (or/c string? false?)]
                           [port-number port-number?]
                           [max-wait-count exact-positive-integer?]
                           [reusable boolean?]
                           [client-connected (-> input-port? output-port? any)])]))

;; -- Types --

(struct listener-config (interface
                         port-number
                         max-wait-count
                         reusable
                         client-connected)
  #:transparent)

;; -- Public Procedures --

(define-thread
  listener
  listener-config
  listener-proc)

;; -- Private Procedures --

;; Main thread procedure for the listener.
(define (listener-proc config)
  (listener-log-trace "Listener thread started.")
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
    (sync-loop ([config config])
               ([(wrapped-thread-receive-evt) listener-proc-thread-message]
                [listener listener-proc-client-connected]))
    ;; Shut down the listener
    (tcp-close listener)
    (listener-log-debug "Listener closed."))
  (listener-log-trace "Listener thread terminating."))

;; Process a received thread message.
(define (listener-proc-thread-message config message)
  (match message
    ;; Shutdown command - stop looping
    ['shutdown (values config #f)]
    ;; Unknown message?
    [else
     (listener-log-error "Received unknown thread message (~A). Ignoring and continuing..." message)
     (values config #t)]))

;; Process a newly connected client.
(define (listener-proc-client-connected config listener)
  (let*-values ([(input-port output-port) (tcp-accept listener)])
    (listener-log-trace "New client accepted.")
    ((listener-config-client-connected config) input-port output-port))
  (values config #t))

;; Local logging procedure.
(define-local-log listener "Listener")
