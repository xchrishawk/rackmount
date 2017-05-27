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

;; -- Provides --

(provide
 (contract-out

  ;; -- Worker Management --

  ;; Create a new worker with the specified identifier.
  [make-worker (-> string? opaque-worker?)]
  ;; Returns the identifier of a worker.
  [worker-identifier (-> opaque-worker? string?)]
  ;; Sends a task to a worker.
  [worker-send-task (-> opaque-worker? any/c void?)]
  ;; Returns the number of active tasks the worker has.
  [worker-task-count (-> opaque-worker? exact-nonnegative-integer?)]
  ;; Terminates the specified worker.
  [worker-terminate (->* (opaque-worker?)
                         (#:finish-tasks boolean?
                          #:synchronous boolean?)
                         void?)]
  ;; Returns a syncable event which is ready after the specified worker is terminated.
  [worker-terminated-evt (-> opaque-worker? evt?)]

  ;; -- Worker Groups --

  ;; Makes N workers with auto-generated identifiers.
  [make-workers (-> exact-positive-integer? (listof opaque-worker?))]
  ;; Sends a task to the worker with the lowest number of active tasks.
  [workers-send-task (-> (listof opaque-worker?) any/c void?)]
  ;; Terminates all of the specified workers.
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

(define (worker-send-task worker task)
  (place-channel-put (opaque-worker-channel worker) task))

(define (worker-task-count worker)
  (place-channel-put/get (opaque-worker-channel worker) 'task-count))

(define (worker-terminate worker
                          #:finish-tasks [finish-tasks #t]
                          #:synchronous [synchronous #t])
  (place-channel-put (opaque-worker-channel worker)
                     (if finish-tasks
                         'terminate-after-completion
                         'terminate-immediately))
  (when synchronous
    (sync (worker-terminated-evt worker)))
  (void))

(define (worker-terminated-evt worker)
  (place-dead-evt (opaque-worker-place worker)))

;; -- Public Procedures (Worker Groups) --

(define (make-workers count)
  (for/list ([n (in-range count)])
    (let ([identifier (format "Worker ~A" n)])
      (make-worker identifier))))

(define (workers-send-task workers task)
  (let ([worker (worker-with-lowest-task-count workers)])
    (worker-send-task worker task)))

(define (workers-terminate workers
                           #:finish-tasks [finish-tasks #t]
                           #:synchronous [synchronous #t])
  ;; Send terminate to all workers immediately
  (for ([worker (in-list workers)])
    (worker-terminate worker #:finish-tasks finish-tasks #:synchronous #f))
  ;; Wait for them all to complete
  (when synchronous
    (for ([worker (in-list workers)])
      (sync (worker-terminated-evt worker)))))

;; -- Private Procedures --

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

  (require "client.rkt")
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

  (define (main)
    (worker-log "Worker launched, waiting for tasks...")
    ;; Enter the main loop for this worker
    (let ([remaining-tasks
           (let loop ([tasks (set)] [terminating #f])
             ;; Wait for an event to occur
             (let* ([task-completed-evts (map gen-task-completed-evt (set->list tasks))]
                    [syncable-evts (cons (worker-channel) task-completed-evts)]
                    [next-evt (apply sync syncable-evts)])
               (match next-evt

                 ;; 'terminate-immediately message - immediately quit and return set of threads
                 ['terminate-immediately
                  (when (not (set-empty? tasks))
                    (worker-log "Warning: terminating while there are still ~A tasks active!"
                                (set-count tasks)))
                  tasks]

                 ;; 'terminate-after-completion message - quit after all tasks complete
                 ['terminate-after-completion
                  (cond
                    ;; No active tasks, OK to shut down immediately
                    [(set-empty? tasks) tasks]
                    ;; At least one active task, need to wait for them to terminate
                    [else
                     (worker-log "Terminating after ~A tasks complete..." (set-count tasks))
                     (loop tasks #t)])]

                 ;; Task-count message - reply with number of active task threads
                 ['task-count
                  (place-channel-put (worker-channel) (set-count tasks))
                  (loop tasks terminating)]

                 ;; Task terminated - remove it from our list
                 [(? (λ (evt) (set-member? tasks evt)) task)
                  (let ([new-tasks (set-remove tasks task)])
                    (cond
                      ;; We are terminating *and* our last task just finished. Close the worker.
                      [(and terminating (set-empty? new-tasks)) new-tasks]
                      ;; Either we're not terminating or there are still tasks left. Keep going.
                      [else (loop new-tasks terminating)]))]

                 ;; New client connected
                 [(list 'client (? input-port? input-port) (? output-port? output-port))
                  (let ([client-task (make-client-task input-port output-port)])
                    (gen-task-start client-task)
                    (loop (set-add tasks client-task) terminating))]

                 ;; Unknown event type - log and ignore
                 [else
                  (worker-log "Received event (~A), don't know what to do with it - discarding." next-evt)
                  (loop tasks terminating)])))])

      ;; Immediately kill any remaining threads
      (when (not (set-empty? remaining-tasks))
        (worker-log "Immediately terminating ~A tasks." (set-count remaining-tasks))
        (for ([task (in-set remaining-tasks)])
          (gen-task-cancel task)
          (sync (gen-task-completed-evt task))))

      ;; Log shutdown
      (worker-log "Worker terminated.")))

  (define (wait-for-task)
    (sync (worker-channel)))

  (define (start-heavy-crunch)
    (thread (λ ()
              (worker-log "heavy crunch whoa!")
              (let loop ([n (random 4294967087)])
                (when (not (zero? n))
                  (loop (sub1 n))))
              (worker-log "that was hard"))))

  (define (worker-log fmt . v)
    (apply rackmount-log (worker-identifier) fmt v)))
