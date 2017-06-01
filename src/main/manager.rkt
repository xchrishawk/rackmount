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

;; -- Private Procedures (Main Loop) --

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
           (loop (manager-proc-task-handle state task-handle))]
          ;; Message from worker
          [(list 'worker-message (? string? worker-identifier) message)
           (loop (manager-proc-worker-message state worker-identifier message))]
          ;; Shutdown command
          ['shutdown (manager-proc-shutdown state)]
          ;; Unknown event? Log and continue
          [else (loop (manager-proc-unknown state))])))
  ;; Log and shut down
  (manager-log-trace "Manager thread terminating.")))

;; -- Private Procedures (Event Handlers) --

;; Process a received task handle.
(define (manager-proc-task-handle state task-handle)
  (let ([task-handle-list (gen:task-handle->list task-handle)]
        [worker (manager-state-select-worker state)])
    (manager-log-trace "Queuing task ~A on worker ~A..."
                       (gen:task-handle-identifier task-handle)
                       (worker-identifier worker))
    (worker-put worker task-handle-list)
    (manager-state-add-task-handle state (worker-identifier worker) task-handle)))

;; Process a message received from a worker.
(define (manager-proc-worker-message state worker-identifier message)
  (match message
    ;; Message is a logged event
    [(? log-event-list? log-event-list)
     (manager-proc-worker-message-log-event state worker-identifier log-event-list)]
    ;; Message is a "task completed" message
    [(list 'task-completed task-handle-identifier)
     (manager-proc-worker-message-task-completed state worker-identifier task-handle-identifier)]
    ;; Unknown message type?
    [else (manager-proc-worker-message-unknown state message)]))

;; Process a shutdown command.
(define (manager-proc-shutdown state)
  (for ([worker (in-list (manager-state-get-workers state))])
    (worker-stop worker)))

;; Process an unknown event.
(define (manager-proc-unknown state evt)
  (manager-log-error "Received unknown event (~A). Ignoring and continuing..." evt)
  state)

;; -- Private Procedures (Worker Message Handlers) --

;; Process a log event message.
(define (manager-proc-worker-message-log-event state worker-identifier log-event-list)
  (let ([log-event (list->log-event log-event-list)])
    (log-event-enqueue log-event)
    state))

;; Process a task completed message.
(define (manager-proc-worker-message-task-completed state worker-identifier task-handle-identifier)
  (let ([task-handle (manager-state-get-task-handle state worker-identifier task-handle-identifier)])
    (manager-log-trace "Received completion of task ~A from worker ~A. Closing and removing..."
                       task-handle-identifier
                       worker-identifier)
    (gen:task-handle-close task-handle)
    (manager-state-remove-task-handle state worker-identifier task-handle-identifier)))

(define (manager-proc-worker-message-unknown state message)
  (manager-log-error "Received unknown message (~A). Ignoring and continuing..." message)
  state)

;; -- Private Procedures (Utility) --

;; Local logging procedure.
(define-local-log manager "Manager")
