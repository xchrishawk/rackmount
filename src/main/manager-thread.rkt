;;
;; manager-thread.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Defines the manager thread, which is responsible for coordinating a set of
;; worker places which provide parallelism for the server.
;;

#lang racket

;; -- Requires --

(require "../main/define-thread.rkt")
(require "../worker/worker.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Configuration struct for the manager thread.
  [struct manager-thread-config ([worker-count exact-positive-integer?])]))

;; -- Types --

(struct manager-thread-config (worker-count)
  #:transparent)

;; -- Public Procedures --

(define-thread
  manager-thread
  manager-thread-config
  manager-thread-proc)

;; -- Private Procedures --

;; Main procedure for the thread.
(define (manager-thread-proc config)

  (manager-log-trace "Manager thread started.")

  ;; Launch workers
  (let* ([workers (for/list ([index (in-range (manager-thread-config-worker-count config))])
                    (let ([identifier (format "Worker ~A" index)])
                      (manager-log-trace "Starting worker with identifier ~A..." identifier)
                      (worker-start identifier)))]
         [worker-get-evts (map worker-get-evt workers)])

    ;; Enter main loop
    (let loop ()

      ;; Get the next event
      (let ([evt (apply sync (wrapped-thread-receive-evt) worker-get-evts)])
        (match evt

          ;; Message from worker
          [(list 'worker-message (? string? identifier) message)

           ;; Check type of message...
           (match message

             ;; Wrapped log event - re-enqueue in main log queue
             [(? log-event-list? lst)
              (let ([log-event (list->log-event lst)])
                (log-event-enqueue log-event))]

             ;; Unknown message type?
             [else
              (manager-log-error "Received unknown message (~A). Ignoring..." message)])

           (loop)]

          ;; Received shutdown event - stop workers and stop looping
          ['shutdown
           (for ([worker (in-list workers)])
             (manager-log-trace "Terminating worker with identifier ~A..." (worker-identifier worker))
             (worker-stop worker))]

          ;; Unknown event? Log and continue
          [else
           (manager-log-error "Received unknown event (~A). Ignoring..." evt)
           (loop)])))

  ;; Log and shut down
  (manager-log-trace "Manager thread terminating.")))

;; Local logging procedure.
(define-local-log manager "Manager")
