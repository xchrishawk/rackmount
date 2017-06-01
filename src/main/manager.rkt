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
(require "../main/manager-state.rkt")
(require "../worker/task.rkt")
(require "../worker/worker.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Configuration struct for the manager thread.
  [struct manager-config ([worker-count exact-positive-integer?])]

  ;; Queues a task handle with a worker.
  [manager-queue-task-handle (-> manager? gen:task-handle? void?)]))

;; -- Types --

(struct manager-config (worker-count)
  #:transparent)

;; -- Public Procedures --

(define-thread
  manager
  manager-config
  manager-proc)

(define (manager-queue-task-handle manager task-handle)
  (thread-send (manager-thread manager) task-handle))

;; -- Private Procedures --

;; Main procedure for the thread.
(define (manager-proc config)
  (manager-log-trace "Manager thread started.")
  ;; Launch workers
  (let* ([worker-list (for/list ([index (in-range (manager-config-worker-count config))])
                        (let ([identifier (format "Worker ~A" index)])
                          (manager-log-trace "Starting worker with identifier ~A..." identifier)
                          (worker-start identifier)))]
         [worker-get-evts (map worker-get-evt worker-list)])
    ;; Enter main loop
    (let loop ([state (manager-state worker-list)])
      ;; Get the next event
      (let ([evt (apply sync (wrapped-thread-receive-evt) worker-get-evts)])
        (match evt
          ;; Task to enqueue - select a worker and send it
          [(? gen:task-handle? task-handle)
           (let ([task-handle-list (gen:task-handle->list task-handle)]
                 [worker (select-worker worker-list)])
             (manager-log-trace "Queuing task ~A on worker ~A..."
                                (gen:task-handle-identifier task-handle)
                                (worker-identifier worker))
             (worker-put worker task-handle-list)
             (loop (manager-state-add-task-handle state (worker-identifier worker) task-handle)))]
          ;; Message from worker
          [(list 'worker-message (? string? worker-identifier) message)
           ;; Check type of message...
           (match message
             ;; Wrapped log event - re-enqueue in main log queue
             [(? log-event-list? lst)
              (let ([log-event (list->log-event lst)])
                (log-event-enqueue log-event)
                (loop state))]
             ;; Task completed event - close and remove the task handle
             [(list 'task-completed task-handle-identifier)
              (let ([task-handle (manager-state-get-task-handle state worker-identifier task-handle-identifier)])
                (manager-log-trace "Received completion of task ~A from worker ~A, closing and removing..."
                                   task-handle-identifier
                                   worker-identifier)
                (gen:task-handle-close task-handle)
                (loop (manager-state-remove-task-handle state worker-identifier task-handle-identifier)))]
             ;; Unknown message type?
             [else
              (manager-log-error "Received unknown message (~A). Ignoring..." message)
              (loop state)])]
          ;; Received shutdown event - stop workers and stop looping
          ['shutdown
           (for ([worker (in-list worker-list)])
             (manager-log-trace "Terminating worker with identifier ~A..." (worker-identifier worker))
             (worker-stop worker))]
          ;; Unknown event? Log and continue
          [else
           (manager-log-error "Received unknown event (~A). Ignoring..." evt)
           (loop state)])))
  ;; Log and shut down
  (manager-log-trace "Manager thread terminating.")))

;; -- Private Utility --

;; Selects the next worker to receive a task.
(define (select-worker worker-list)
  ;; For now, just pick a random one. Eventually this should be a priority queue.
  (list-ref worker-list (random (length worker-list))))

;; Local logging procedure.
(define-local-log manager "Manager")
