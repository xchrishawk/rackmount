#lang racket

;; -- Requires --

(require "log.rkt")

;; -- Provides --

(provide
 (contract-out
  [server-start (-> string? (or/c string? false/c) exact-positive-integer? server?)]
  [server-stop (-> server? void?)]))

;; -- Types --

;; Opaque type representing a server instance.
(struct server (listener-thread))

;; -- Macros --

(define-syntax-rule (thread-start expr0 expr ...)
  (thread (λ () expr0 expr ...)))

;; -- Public Procedures --

;; Starts a server instance.
(define (server-start working-dir hostname port)
  (let ([listener-thread (launch-listener-thread hostname port)])
    (server listener-thread)))

;; Stops a server instance.
(define (server-stop srv)
  (shutdown-thread (server-listener-thread srv)))

;; -- Private Procedures --

;; Launches and returns a listener thread.
(define (launch-listener-thread hostname port)
  (define log (create-local-log "Listener"))
  (thread-start
   ;; Launch listener
   (let ([listener (tcp-listen port 4 #t hostname)])
     ;; Log startup
     (log "Listener thread launched - listening on ~A port ~A..."
          (or hostname "any interface")
          port)
     ;; Loop until we receive a shutdown event
     (let ([remaining-client-threads
            (let loop ([client-threads (set)])
              ;; Wait for an event
              (let* ([syncable-evts (flatten (list listener
                                                   (set->list client-threads)
                                                   (thread-receive-evt)))]
                     [this-evt (apply sync syncable-evts)])
                (cond
                  ;; Event is listener - we received a new client
                  [(equal? listener this-evt)
                   (let-values ([(input output) (tcp-accept listener)])
                     (let ([client-thread (launch-client-thread input output)])
                       (loop (set-add client-threads client-thread))))]
                  ;; Event is a thread - client disconnected
                  [(set-member? client-threads this-evt)
                   (loop (set-remove client-threads this-evt))]
                  ;; Event is shutdown - bail from loop and return remaining client threads
                  [(event-is-shutdown this-evt) client-threads]
                  ;; Unknown event - crash the app
                  [else (error "Unknown event?")])))])
       ;; Terminate all of the remaining client threads
       (when (not (set-empty? remaining-client-threads))
         (for ([client-thread (in-set remaining-client-threads)])
           (shutdown-thread client-thread))
         (log "Warning: Terminated ~A active client threads." (set-count remaining-client-threads)))
       ;; Close listener and log shutdown
       (tcp-close listener)
       (log "Listener thread terminated.")))))

;; Launches and returns a client thread.
(define (launch-client-thread input output)
  (define log (create-local-log "Client"))
  (thread-start
   ;; Log client connection
   (log "Client connected...")
   ;; Loop until getting a shutdown event
   (let loop ()
     (let* ([syncable-evts (list (read-line-evt input 'any)
                                 (thread-receive-evt))]
            [this-evt (apply sync syncable-evts)])
       (cond
         ;; Event is a string - input received from client
         [(string? this-evt)
          (log "RX: ~A" this-evt)
          (loop)]
         ;; Event is EOF - client disconnected
         [(equal? eof this-evt) (void)]
         ;; Event is shudown - bail from loop
         [(event-is-shutdown this-evt) (void)]
         ;; Unknown event?
         [else (error "Unknown event?")])))
   ;; Terminate the connection and log
   (close-input-port input)
   (close-output-port output)
   (log "Client disconnected.")))

;; Synchronously kills and waits for a thread to terminate.
(define (shutdown-thread t)
  (thread-send t 'shutdown)
  (sync t)
  (void))

;; Returns #t if the specified event is a shutdown event.
(define (event-is-shutdown evt)
  (and (equal? evt (thread-receive-evt))
       (equal? (thread-receive) 'shutdown)))
