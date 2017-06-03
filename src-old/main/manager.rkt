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
  (let loop ([state (make-manager-state config)])
    (sync
     ;; Received thread message
     (handle-evt
      (wrapped-thread-receive-evt)
      (match-lambda
        ;; Task to enqueue
        [(? gen:task-handle? task-handle)
         (let ([task-handle-list (gen:task-handle->list task-handle)]
               [worker (manager-state-select-worker state)])
           (manager-log-trace
            "Delegating task ~A to worker ~A..."
            (gen:task-handle-identifier task-handle)
            (worker-identifier worker))
           (worker-put worker task-handle-list)
           (loop (manager-state-add-task-handle
                  state
                  (worker-identifier worker)
                  task-handle)))]
        ;; Shutdown command - stop looping
        ['shutdown (void)]
        ;; Unknown message?
        [unrecognized-message
         (manager-log-error
          "Received unknown thread message (~A). Ignoring and continuing..."
          unrecognized-message)
         (loop state)]))
     ;; Received worker message
     (handle-evt
      (apply choice-evt (manager-state-worker-get-evts state))
      (match-lambda
        ;; Log event from worker
        [(list 'worker-message
               (? worker-identifier? worker-identifier)
               (? log-event-list? log-event-list))
         (let ([log-event (list->log-event log-event-list)])
           (log-event-enqueue log-event)
           (loop state))]
        ;; Task completed
        [(list 'worker-message
               (? worker-identifier? worker-identifier)
               (list 'task-completed
                     (? gen:task-identifier? task-identifier)))
         (let ([task-handle (manager-state-get-task-handle
                             state
                             worker-identifier
                             task-identifier)])
           (manager-log-trace
            "Received completion for task ~A, closing it."
            task-identifier)
           (gen:task-handle-close task-handle)
           (loop (manager-state-remove-task-handle
                  state
                  worker-identifier
                  task-identifier)))]
        ;; Unrecognized message
        [unrecognized-message
         (manager-log-error
          "Received unknown worker message (~A). Ignoring and continuing..."
          unrecognized-message)
         (loop state)]))))
  ;; Log shutdown
  (manager-log-trace "Manager thread terminating."))

;; -- Private Procedures --

;; Builds the manager state object with the specified number of workers.
(define (make-manager-state config)
  (manager-state
   (for/list ([index (in-range (manager-config-worker-count config))])
     (let ([identifier (format "Worker ~A" index)])
       (manager-log-trace "Starting worker with identifier ~A..." identifier)
       (worker-start identifier)))))

;; Local logging procedure.
(define-local-log manager "Manager")
