;;
;; manager.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Defines the manager thread, which is responsible for coordinating a set of
;; worker places which provide parallelism for the server.
;;

#lang racket

;; -- Requires --

(require "../main/define-thread.rkt")
(require "../worker/task.rkt")
(require "../worker/worker.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Configuration struct for the manager thread.
  [struct manager-config ([worker-count exact-positive-integer?])]

  ;; Queues a client task with a worker.
  [manager-queue-task (-> manager? gen:task? void?)]))

;; -- Types --

(struct manager-config (worker-count)
  #:transparent)

;; -- Public Procedures --

(define-thread
  manager
  manager-config
  manager-proc)

(define (manager-queue-task manager task)
  (thread-send (manager-thread manager) task))

;; -- Private Procedures --

;; Main procedure for the thread.
(define (manager-proc config)
  (manager-log-trace "Manager thread started.")
  ;; Launch workers
  (let* ([workers (for/list ([index (in-range (manager-config-worker-count config))])
                    (let ([identifier (format "Worker ~A" index)])
                      (manager-log-trace "Starting worker with identifier ~A..." identifier)
                      (worker-start identifier)))]
         [worker-get-evts (map worker-get-evt workers)])
    ;; Enter main loop
    (let loop ()
      ;; Get the next event
      (let ([evt (apply sync (wrapped-thread-receive-evt) worker-get-evts)])
        (match evt
          ;; Task to enqueue - select a worker and send it
          [(? gen:task? task)
           (let ([task-list (gen:task->list task)]
                 [worker (select-worker workers)])
             (worker-put worker task-list))
           (loop)]
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

(define (select-worker workers)
  ;; For now, just pick a random one. Eventually this should be a priority queue.
  (list-ref workers (random (length workers))))

;; Local logging procedure.
(define-local-log manager "Manager")
