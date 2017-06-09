;;
;; listener.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the listener thread, which is responsible for listening
;; for incoming TCP connections and starting new clients.
;;

#lang racket

;; -- Requires --

(require "../util/exceptions.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

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
  (let* ([startup-semaphore (make-semaphore)]
         [thd (thread-start (listener-proc config startup-semaphore))])
    (semaphore-wait startup-semaphore)
    (listener thd)))

(define (listener-terminate listener #:synchronous [synchronous #t])
  (let ([thd (listener-thread listener)])
    (thread-send thd 'terminate)
    (when synchronous
      (sync thd)))
  (void))

;; -- Private Procedures --

;; Main thread procedure for the listener.
(define (listener-proc config startup-semaphore)
  (listener-log-trace "Thread running.")
  ;; Open listener
  (let ([listener (tcp-listen (listener-config-port-number config)
                              (listener-config-max-wait-count config)
                              (listener-config-reusable config)
                              (listener-config-interface config))])
    ;; Notify caller that we're running
    (semaphore-post startup-semaphore)
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
          [bad-message (raise-bad-message-error bad-message)]))
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

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)

  ;; -- Test Case --

  (test-case "Lifecycle"

    (parameterize ([minimum-log-event-level 'critical])

      ;; Constants
      (define port-number 44444)
      (define client-connected-semaphore (make-semaphore))

      ;; Server ports
      (define server-input-port #f)
      (define server-output-port #f)

      ;; Helper function
      (define (verify-cannot-connect)
        (check-exn
         exn:fail:network?
         (thunk
          (tcp-connect "localhost" port-number))))

      ;; Verify we can't connect initially
      (verify-cannot-connect)

      ;; Create listener
      (let* ([config (listener-config #f port-number 4 #t
                                      (λ (input-port output-port)
                                        (set! server-input-port input-port)
                                        (set! server-output-port output-port)
                                        (semaphore-post client-connected-semaphore)))]
             [listener (make-listener config)])
        ;; Connect
        (let-values ([(client-input-port client-output-port)
                      (tcp-connect "localhost" port-number)])
          ;; Verify closure got called
          (check-equal?
           (sync/timeout 5.0 client-connected-semaphore)
           client-connected-semaphore)
          (check-true (input-port? server-input-port))
          (check-true (output-port? server-output-port))
          ;; Verify 2-way communication
          (define (check-comm input-port output-port)
            (let ([message "hello"])
              (displayln message output-port)
              (flush-output output-port)
              (check-equal? (read-line input-port) message)))
          (check-comm server-input-port client-output-port)
          (check-comm client-input-port server-output-port)
          ;; Disconnect
          (close-input-port client-input-port)
          (close-output-port client-output-port)
          (close-input-port server-input-port)
          (close-output-port server-output-port))

        ;; Terminate the listener
        (listener-terminate listener))

      ;; Verify we can no longer connect
      (verify-cannot-connect))))
