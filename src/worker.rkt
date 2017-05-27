;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines worker places, which are used to implement parallelism
;; for the server. Each worker represents a Racket "place", which is a separate
;; instance of the Racket runtime environment that can run fully in parallel
;; with other places.
;;

#lang racket

;; -- Requires --

(require syntax/location)
(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; -- Worker Management --

  ;; Create a new worker with the specified identifier.
  [make-worker (-> string? opaque-worker?)]

  ;; Returns the identifier of a worker.
  [worker-identifier (-> opaque-worker? string?)]

  ;; Queues a task on a worker.
  [worker-queue-task (-> opaque-worker? task-spec? void?)]

  ;; Returns the number of active tasks the worker has.
  [worker-task-count (-> opaque-worker? exact-nonnegative-integer?)]

  ;; Terminates the specified worker.
  ;;
  ;; If #:finish-tasks is set to #t (the default), all active tasks will be allowed
  ;; to complete normally before the place will terminate. Otherwise, all active
  ;; tasks will be immediately cancelled.
  ;;
  ;; If #:synchronous is set to #t (the default), this method will block until the
  ;; worker place has terminated by syncing on the (worker-terminated-evt) event.
  [worker-terminate (->* (opaque-worker?)
                         (#:finish-tasks boolean?
                          #:synchronous boolean?)
                         void?)]

  ;; Returns a syncable event which is ready after the specified worker has
  ;; completely terminated. The result of the synchronization event is the worker
  ;; itself.
  [worker-terminated-evt (-> opaque-worker? evt?)]

  ;; -- Worker Groups --

  ;; Makes N workers with auto-generated identifiers.
  [make-workers (-> exact-positive-integer? (listof opaque-worker?))]

  ;; Queues a task on the worker with the lowest number of active tasks.
  [workers-queue-task (-> (listof opaque-worker?) task-spec? void?)]

  ;; Terminates all of the specified workers. See (worker-terminate) for a
  ;; description of the arguments. Note that all workers will be commanded to
  ;; terminate prior to syncing on any worker's terminated event.
  [workers-terminate (->* ((listof opaque-worker?))
                          (#:finish-tasks boolean?
                           #:synchronous boolean?)
                          void?)]))

;; -- Structs --

;; Opaque struct containing worker data.
(struct opaque-worker (identifier place channel))

;; -- Public Procedures (Worker Management) --

(define (make-worker identifier)
  (let* ([worker-place (dynamic-place (quote-module-path worker-module) 'start)]
         [worker-channel (place-channel-get worker-place)])
    (place-channel-put worker-place identifier)
    (opaque-worker identifier worker-place worker-channel)))

(define (worker-identifier worker)
  (opaque-worker-identifier worker))

(define (worker-queue-task worker task)
  (place-channel-put (opaque-worker-channel worker) task))

(define (worker-task-count worker)
  (place-channel-put/get (opaque-worker-channel worker) 'task-count))

(define-void-return (worker-terminate worker
                                      #:finish-tasks [finish-tasks #t]
                                      #:synchronous [synchronous #t])
  (place-channel-put (opaque-worker-channel worker)
                     (if finish-tasks
                         'terminate-after-completion
                         'terminate-immediately))
  (when synchronous
    (sync (worker-terminated-evt worker))))

(define (worker-terminated-evt worker)
  (wrap-evt
   (place-dead-evt (opaque-worker-place worker))
   (λ (evt) worker)))

;; -- Public Procedures (Worker Groups) --

(define (make-workers count)
  (for/list ([n (in-range count)])
    (let ([identifier (format "Worker ~A" n)])
      (make-worker identifier))))

(define-void-return (workers-queue-task workers task)
  (let ([worker (worker-with-lowest-task-count workers)])
    (worker-queue-task worker task)))

(define-void-return (workers-terminate workers
                                       #:finish-tasks [finish-tasks #t]
                                       #:synchronous [synchronous #t])
  (for ([worker (in-list workers)])
    (worker-terminate worker #:finish-tasks finish-tasks #:synchronous #f))
  (when synchronous
    (for ([worker (in-list workers)])
      (sync (worker-terminated-evt worker)))))

;; -- Private Procedures --

;; Returns true if the argument is allowed as a task specifier.
(define (task-spec? x)
  (place-message-allowed? x))

;; Returns the worker from the list with the lowest task count.
(define (worker-with-lowest-task-count workers)
  (let loop ([workers workers] [current-worker #f] [current-count #f])
    (if (null? workers)
        current-worker
        (let* ([this-worker (first workers)]
               [this-count (worker-task-count this-worker)])
          (cond
            ;; If worker has zero tasks, no point in checking any other worker
            [(zero? this-count) this-worker]
            ;; This worker is the lowest
            [(or (not current-worker)
                 (< this-count current-count))
             (loop (rest workers) this-worker this-count)]
            ;; This worker is not the lowest
            [else (loop (rest workers) current-worker current-count)])))))

;; -- Worker Module --

(module worker-module racket

  ;; -- Requires --

  (require "client-task.rkt")
  (require "log.rkt")
  (require "task.rkt")

  ;; -- Provides --

  (provide
   (contract-out
    [start (-> place-channel? any)]))

  ;; -- Parameters --

  (define worker-identifier (make-parameter #f))
  (define worker-channel (make-parameter #f))

  ;; -- Startup Procedure --

  ;; Startup procedure - exchanges identifier and channel information with caller
  (define (start bootstrap-channel)
    (let*-values ([(our-channel their-channel) (place-channel)]
                  [(identifier) (place-channel-put/get bootstrap-channel their-channel)])
      (parameterize ([worker-identifier identifier]
                     [worker-channel our-channel])
        (main))))

  ;; -- Private Procedures --

  ;; Main procedure.
  (define (main)
    (worker-log "Worker launched, waiting for tasks...")
    ;; Enter the main loop for this worker
    (let ([remaining-tasks (main-loop)])

      ;; Cancel any remaining tasks
      (when (not (null? remaining-tasks))
        (worker-log "WARNING: Terminating ~A active tasks..." (length remaining-tasks))
        (for ([task (in-list remaining-tasks)])
          (gen-task-cancel task #:synchronous #f))
        (apply sync (map gen-task-completed-evt remaining-tasks)))

      ;; Log shutdown
      (worker-log "Worker terminated.")))

  ;; Main loop - returns any active tasks left after shutdown.
  (define (main-loop)
    (set->list
     (let loop ([tasks (set)] [terminating #f])

       ;; Wait for next event
       (let* ([task-list (set->list tasks)]
              [task-completed-evts (map gen-task-completed-evt task-list)]
              [syncable-evts (cons (worker-channel) task-completed-evts)])
         (match (apply sync syncable-evts)

           ;; Return immediately without waiting for tasks to complete
           ['terminate-immediately
            (worker-log "Immediate termination requested.")
            tasks]

           ;; Wait for all tasks to complete normally, then terminate
           ['terminate-after-completion
            (cond
              ;; No tasks remaining, we can terminate immediately
              [(set-empty? tasks)
               (worker-log "Normal termination requested - no active tasks.")
               tasks]
              ;; At least one task remaining, wait for all to complete
              [else
               (worker-log "Normal termination requested - waiting on ~A tasks..." (set-count tasks))
               (loop tasks #t)])]

           ;; Request for number of active tasks - reply and keep looping
           ['task-count
            (place-channel-put (worker-channel) (set-count tasks))
            (loop tasks terminating)]

           ;; Task completed
           [(? (λ (evt) (set-member? tasks evt)) task)
            (let-values ([(new-tasks continue) (complete-task task tasks terminating)])
              (if continue
                  (loop new-tasks terminating)
                  new-tasks))]

           ;; New task spec
           [(app task-from-task-spec (? task? task))
            (let ([new-tasks (queue-task task tasks terminating)])
              (loop new-tasks terminating))]

           ;; An unknown event? Log it and keep going...
           [else
            (worker-log "WARNING: Received unknown event (~A) - ignoring and continuing...")
            (loop tasks terminating)])))))

  ;; Queues the specified task. Returns the new set of active tasks.
  (define (queue-task task tasks terminating)
    (if terminating
        (begin
          (gen-task-reject task)
          (worker-log "WARNING: Rejecting task because worker is terminating.")
          tasks)
        (let ([new-tasks (set-add tasks task)])
          (worker-log "Task started, ~A tasks now active." (set-count new-tasks))
          (gen-task-start task)
          new-tasks)))

  ;; Completes the specified task. Returns the new set of active tasks.
  (define (complete-task task tasks terminating)
    (let* ([new-tasks (set-remove tasks task)]
           [continue (not (and terminating (set-empty? new-tasks)))])
      (worker-log "Task completed, ~A tasks now active." (set-count new-tasks))
      (values new-tasks continue)))

  ;; Gets a task from the specified task spec.
  (define (task-from-task-spec task-spec)
    (match task-spec
      ;; Client connected
      [(list 'client (? input-port? input-port) (? output-port? output-port))
       (make-client-task input-port output-port)]
      ;; Don't know what this is - ignore it
      [else #f]))

  ;; Logs an event to this worker's category.
  (define (worker-log fmt . v)
    (apply rackmount-log (worker-identifier) fmt v)))
