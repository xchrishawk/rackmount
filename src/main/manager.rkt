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
    (let loop ([worker-hash (make-worker-hash worker-list)])
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
             (loop (worker-hash-add-task-handle worker-hash
                                                (worker-identifier worker)
                                                task-handle)))]
          ;; Message from worker
          [(list 'worker-message (? string? worker-identifier) message)
           ;; Check type of message...
           (match message
             ;; Wrapped log event - re-enqueue in main log queue
             [(? log-event-list? lst)
              (let ([log-event (list->log-event lst)])
                (log-event-enqueue log-event)
                (loop worker-hash))]
             ;; Task completed event - close and remove the task handle
             [(list 'task-completed task-handle-identifier)
              (let ([task-handle (worker-hash-get-task-handle worker-hash
                                                              worker-identifier
                                                              task-handle-identifier)])
                (manager-log-trace "Received completion of task ~A from worker ~A, closing and removing..."
                                   task-handle-identifier
                                   worker-identifier)
                (gen:task-handle-close task-handle)
                (loop (worker-hash-remove-task-handle worker-hash
                                                      worker-identifier
                                                      task-handle-identifier)))]
             ;; Unknown message type?
             [else
              (manager-log-error "Received unknown message (~A). Ignoring..." message)
              (loop worker-hash)])]
          ;; Received shutdown event - stop workers and stop looping
          ['shutdown
           (for ([worker (in-list worker-list)])
             (manager-log-trace "Terminating worker with identifier ~A..." (worker-identifier worker))
             (worker-stop worker))]
          ;; Unknown event? Log and continue
          [else
           (manager-log-error "Received unknown event (~A). Ignoring..." evt)
           (loop worker-hash)])))
  ;; Log and shut down
  (manager-log-trace "Manager thread terminating.")))

;; -- Private Utility (Worker State Management) --

;; Creates a worker state hash.
(define (make-worker-hash worker-list)
  (for/hash ([worker (in-list worker-list)])
    (values (worker-identifier worker) (hash))))

;; Adds a task handle to the worker state hash.
(define (worker-hash-add-task-handle worker-hash
                                     worker-identifier
                                     task-handle)
  (let ([task-hash (hash-ref worker-hash worker-identifier)])
    (hash-set worker-hash
              worker-identifier
              (hash-set task-hash
                        (gen:task-handle-identifier task-handle)
                        task-handle))))

;; Gets a task handle from the worker state hash.
(define (worker-hash-get-task-handle worker-hash
                                     worker-identifier
                                     task-handle-identifier)
  (let ([task-hash (hash-ref worker-hash worker-identifier)])
    (hash-ref task-hash task-handle-identifier)))

;; Removes a task handle from the worker state hash.
(define (worker-hash-remove-task-handle worker-hash
                                        worker-identifier
                                        task-handle-identifier)
  (let ([task-hash (hash-ref worker-hash worker-identifier)])
    (hash-set worker-hash
              worker-identifier
              (hash-remove task-hash task-handle-identifier))))

;; Selects the next worker to receive a task.
(define (select-worker worker-list)
  ;; For now, just pick a random one. Eventually this should be a priority queue.
  (list-ref worker-list (random (length worker-list))))

;; -- Private Utility (Misc) --

;; Local logging procedure.
(define-local-log manager "Manager")
