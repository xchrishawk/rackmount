;;
;; manager.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the "manager" object, which is responsible for coordinating
;; the activities of a number of worker places.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")
(require "../tasks/worker.rkt")
(require "../util/logging.rkt")
(require "../util/mlhash.rkt")
(require "../util/thread-util.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct encapsulating configuration for a manager.
  [struct manager-config ([worker-count exact-positive-integer?])]

  ;; Creates a new manager with the specified configuration.
  [rename make-manager manager (-> manager-config? manager?)]

  ;; Enqueues a new task with the manager.
  [manager-enqueue (-> manager? gen:task-handle? void?)]

  ;; Synchronously terminates the manager.
  [manager-terminate (-> manager? void?)]))

;; -- Structs --

(struct manager-config (worker-count)
  #:transparent)

(struct manager (thread))

;; -- Public Procedures --

(define (make-manager config)
  (let* ([startup-semaphore (make-semaphore)]
         [thd (thread-start (manager-proc config startup-semaphore))])
    (semaphore-wait startup-semaphore)
    (manager thd)))

(define (manager-enqueue manager task-handle)
  (thread-send
   (manager-thread manager)
   task-handle))

(define (manager-terminate manager
                           #:immediately [immediately #t]
                           #:synchronous [synchronous #t])
  (let ([thd (manager-thread manager)])
    (thread-send thd (list 'terminate immediately))
    (when synchronous
      (sync thd)))
  (void))

;; -- Private Procedures --

(define (manager-proc config startup-semaphore)
  (manager-log-trace "Thread running.")
  ;; Launch workers and notify caller that startup is complete
  (let ([worker-list (make-workers (manager-config-worker-count config))])
    (semaphore-post startup-semaphore)
    ;; Enter main loop
    (let loop ([worker-list worker-list]
               [tasks (mlhash 2)])
      (sync
       ;; Received thread message
       (handle-evt
        (wrap-evt (thread-receive-evt) (thunk* (thread-receive)))
        (match-lambda
          ;; Terminate command - command workers to terminate
          ;; NOTE: It's worth talking about the general strategy here. The worker
          ;;       object has logic for cancelling tasks, rejecting tasks when
          ;;       termination is in progress, etc., so we don't duplicate that
          ;;       here. Instead we just notify the workers they need to terminate
          ;;       (either immediately or non-immediately), and then continue
          ;;       looping as normal until we receive notification that all of the
          ;;       workers have died natural deaths. We then stop looping.
          [(list 'terminate (? boolean? immediately))
           (for ([worker (in-list worker-list)])
             (worker-terminate worker #:immediately immediately #:synchronous #f))
           (loop worker-list tasks)]
          ;; New task handle to enqueue. Select a worker and send it.
          [(? gen:task-handle? task-handle)
           (let ([worker (select-worker worker-list tasks)])
             (manager-log-trace
              (string-append
               "Received task (~A), initializing and enqueueing with ~A. "
               "~A tasks currently active with all workers.")
              (gen:task-handle-identifier task-handle)
              (worker-identifier worker)
              (add1 (mlhash-count tasks)))
             (gen:task-handle-initialize task-handle)
             (worker-enqueue worker task-handle)
             (loop
              worker-list
              (mlhash-set
               tasks
               (worker-identifier worker)
               (gen:task-handle-identifier task-handle)
               task-handle)))]
          ;; Unrecognized message
          [unrecognized-message
           (manager-log-error "Unrecognized thread message (~A). Ignoring..." unrecognized-message)
           (loop worker-list tasks)]))
       ;; Received place message
       (handle-evt
        (apply choice-evt (map worker-get-evt worker-list))
        (match-lambda
          ;; Task completed event - remove it from our tracking list.
          [(list 'task-completed
                 (? worker-identifier? worker-identifier)
                 (? gen:task-identifier? task-identifier))
           (manager-log-trace
            (string-append
             "Received completion for task (~A) from ~A, closing it. "
             "~A tasks currently active with all workers.")
            task-identifier
            worker-identifier
            (sub1 (mlhash-count tasks)))
           (let ([task-handle (mlhash-ref tasks worker-identifier task-identifier)])
             (gen:task-handle-close task-handle)
             (loop worker-list (mlhash-remove tasks worker-identifier task-identifier)))]))
       ;; Received place terminated event - remove it from the worker list, and
       ;; stop looping if all of the workers are dead.
       (handle-evt
        (apply choice-evt (map worker-terminated-evt worker-list))
        (λ (worker)
          (let ([updated-worker-list (remove worker worker-list)])
            (manager-log-trace
             "~A terminated, ~A workers remaining."
             (worker-identifier worker)
             (length updated-worker-list))
            (when (not (null? updated-worker-list))
              (loop updated-worker-list tasks))))))))
  (manager-log-trace "Thread terminated."))

;; Makes n workers.
(define (make-workers n)
  (for/list ([index (in-range n)])
    (let ([identifier (format "Worker ~A" index)])
      (worker identifier))))

;; Selects the worker with the lowest number of active tasks.
(define (select-worker worker-list tasks)
  (argmin
   (λ (worker)
     (mlhash-count tasks (worker-identifier worker)))
   worker-list))

;; Local logging procedure.
(define-local-log manager "Manager")

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)
  (require "../tasks/example-task.rkt")

  ;; -- Test Cases --

  (test-case "Non-Immediate Cancellation"
    (let ([m (make-manager (manager-config 3))])
      (for ([index (in-range 1 7)])
        (manager-enqueue
         m
         (example-task-handle (format "Task ~A" index) (* 1.0 index))))
      (manager-terminate m #:immediately #f)))

  (test-case "Immediate Cancellation"
    (let ([m (make-manager (manager-config 3))])
      (for ([index (in-range 1 7)])
        (manager-enqueue
         m
         (example-task-handle (format "Task ~A" index) (* 5.0 index))))
      (sleep 1.0)
      (manager-terminate m #:immediately #t))))
